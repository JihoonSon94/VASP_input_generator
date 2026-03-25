from __future__ import annotations

import math
import tempfile
from collections import Counter
from pathlib import Path
from typing import Iterable

import numpy as np
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import streamlit as st
from streamlit.runtime.scriptrunner import get_script_run_ctx
from pymatgen.core import Composition, Element, Lattice, Structure
from pymatgen.core.surface import (
    Slab,
    SlabGenerator,
    center_slab,
    get_symmetrically_distinct_miller_indices,
    get_symmetrically_equivalent_miller_indices,
)
from pymatgen.io.vasp import Poscar
from pymatgen.symmetry.analyzer import SpacegroupAnalyzer


RUNNING_IN_STREAMLIT = get_script_run_ctx(suppress_warning=True) is not None


def _identity_cache_data(*args, **kwargs):
    def decorator(func):
        return func

    return decorator


cache_data = st.cache_data if RUNNING_IN_STREAMLIT else _identity_cache_data

if RUNNING_IN_STREAMLIT:
    st.set_page_config(page_title="Slab Studio", layout="wide")


ATOM_RENDER_WARNING_THRESHOLD = 900
BOND_RENDER_THRESHOLD = 260
DEFAULT_LAYER_TOLERANCE = 0.35
PALETTE = px.colors.qualitative.Dark24 + px.colors.qualitative.Alphabet + px.colors.qualitative.Set3
LAYER_PALETTE = px.colors.qualitative.Bold + px.colors.qualitative.Pastel + px.colors.qualitative.Safe


def build_demo_structure(name: str) -> Structure:
    demos: dict[str, Structure] = {
        "Si (diamond)": Structure.from_spacegroup(
            "Fd-3m",
            Lattice.cubic(5.431),
            ["Si"],
            [[0, 0, 0]],
        ),
        "NaCl (rocksalt)": Structure.from_spacegroup(
            "Fm-3m",
            Lattice.cubic(5.6402),
            ["Na", "Cl"],
            [[0, 0, 0], [0.5, 0.5, 0.5]],
        ),
        "SrTiO3 (perovskite)": Structure.from_spacegroup(
            "Pm-3m",
            Lattice.cubic(3.905),
            ["Sr", "Ti", "O"],
            [[0, 0, 0], [0.5, 0.5, 0.5], [0.5, 0.5, 0]],
        ),
        "TiO2 (rutile)": Structure.from_spacegroup(
            "P42/mnm",
            Lattice.tetragonal(4.5937, 2.9587),
            ["Ti", "O"],
            [[0, 0, 0], [0.305, 0.305, 0]],
        ),
    }
    return demos[name].copy()


def load_structure_from_upload(uploaded_file, parser_hint: str) -> Structure:
    raw = uploaded_file.getvalue()
    filename = Path(uploaded_file.name).name or "uploaded_structure"

    if parser_hint == "auto":
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp_path = Path(tmpdir) / filename
            tmp_path.write_bytes(raw)
            return Structure.from_file(tmp_path)

    text = raw.decode("utf-8", errors="ignore")
    return Structure.from_str(text, fmt=parser_hint)


def normalize_for_surface(
    structure: Structure,
    use_conventional_cell: bool,
    symprec: float,
) -> Structure:
    if not use_conventional_cell:
        return structure.copy()

    analyzer = SpacegroupAnalyzer(structure, symprec=symprec)
    return analyzer.get_conventional_standard_structure(keep_site_properties=True)


@cache_data(show_spinner=False)
def normalize_structure_cached(
    structure_dict: dict,
    use_conventional_cell: bool,
    symprec: float,
) -> dict:
    structure = Structure.from_dict(structure_dict)
    normalized = normalize_for_surface(structure, use_conventional_cell, symprec)
    return normalized.as_dict()


def format_miller(miller: tuple[int, int, int]) -> str:
    return f"({miller[0]} {miller[1]} {miller[2]})"


def sanitize_miller(h: int, k: int, l: int) -> tuple[int, int, int]:
    miller = (int(h), int(k), int(l))
    if miller == (0, 0, 0):
        raise ValueError("Miller index (0, 0, 0) is not valid.")
    return miller


def get_facet_candidates(structure: Structure, max_index: int) -> list[tuple[int, int, int]]:
    candidates = get_symmetrically_distinct_miller_indices(
        structure,
        max_index=max_index,
        return_hkil=False,
    )
    return sorted(candidates, key=lambda x: (sum(abs(i) for i in x), x))


@cache_data(show_spinner=False)
def facet_records_cached(structure_dict: dict, max_index: int) -> list[dict]:
    structure = Structure.from_dict(structure_dict)
    rows: list[dict] = []
    for facet in get_facet_candidates(structure, max_index=max_index):
        equivalent = get_symmetrically_equivalent_miller_indices(
            structure,
            facet,
            return_hkil=False,
        )
        rows.append(
            {
                "facet": facet,
                "facet_label": format_miller(facet),
                "d_hkl(A)": round(float(structure.lattice.d_hkl(facet)), 4),
                "equivalent_facets": len(equivalent),
                "family": ", ".join(format_miller(idx) for idx in equivalent),
            }
        )
    return rows


def slab_generation_kwargs(
    structure: Structure,
    miller: tuple[int, int, int],
    thickness_mode: str,
    slab_size_value: float,
    vacuum_value: float,
) -> tuple[float, float, bool, dict[str, float | int | bool]]:
    d_hkl = float(structure.lattice.d_hkl(miller))

    if thickness_mode == "Angstrom":
        min_slab_size = float(slab_size_value)
        min_vacuum_size = float(vacuum_value)
        in_unit_planes = False
    else:
        min_slab_size = int(round(slab_size_value))
        min_vacuum_size = max(1, math.ceil(float(vacuum_value) / d_hkl))
        in_unit_planes = True

    meta = {
        "d_hkl": d_hkl,
        "min_slab_size": min_slab_size,
        "min_vacuum_size": min_vacuum_size,
        "in_unit_planes": in_unit_planes,
    }
    return min_slab_size, min_vacuum_size, in_unit_planes, meta


def build_slab_generator(
    structure: Structure,
    miller: tuple[int, int, int],
    min_slab_size: float,
    min_vacuum_size: float,
    in_unit_planes: bool,
    lll_reduce: bool,
    center_the_slab: bool,
    primitive: bool,
    max_normal_search: int,
) -> SlabGenerator:
    return SlabGenerator(
        initial_structure=structure,
        miller_index=miller,
        min_slab_size=min_slab_size,
        min_vacuum_size=min_vacuum_size,
        lll_reduce=lll_reduce,
        center_slab=center_the_slab,
        in_unit_planes=in_unit_planes,
        primitive=primitive,
        max_normal_search=max_normal_search,
    )


@cache_data(show_spinner=False)
def generate_slabs_cached(
    structure_dict: dict,
    miller: tuple[int, int, int],
    min_slab_size: float,
    min_vacuum_size: float,
    in_unit_planes: bool,
    lll_reduce: bool,
    center_the_slab: bool,
    primitive: bool,
    max_normal_search: int,
    symmetrize: bool,
    ftol: float,
    tol: float,
    filter_out_sym_slabs: bool,
) -> list[dict]:
    structure = Structure.from_dict(structure_dict)
    slabgen = build_slab_generator(
        structure=structure,
        miller=miller,
        min_slab_size=min_slab_size,
        min_vacuum_size=min_vacuum_size,
        in_unit_planes=in_unit_planes,
        lll_reduce=lll_reduce,
        center_the_slab=center_the_slab,
        primitive=primitive,
        max_normal_search=max_normal_search,
    )
    slabs = slabgen.get_slabs(
        ftol=ftol,
        tol=tol,
        symmetrize=symmetrize,
        filter_out_sym_slabs=filter_out_sym_slabs,
    )
    return [slab.as_dict() for slab in slabs]


@cache_data(show_spinner=False)
def generate_single_slab_cached(
    structure_dict: dict,
    miller: tuple[int, int, int],
    min_slab_size: float,
    min_vacuum_size: float,
    in_unit_planes: bool,
    lll_reduce: bool,
    center_the_slab: bool,
    primitive: bool,
    max_normal_search: int,
    shift: float,
    tol: float,
) -> dict:
    structure = Structure.from_dict(structure_dict)
    slabgen = build_slab_generator(
        structure=structure,
        miller=miller,
        min_slab_size=min_slab_size,
        min_vacuum_size=min_vacuum_size,
        in_unit_planes=in_unit_planes,
        lll_reduce=lll_reduce,
        center_the_slab=center_the_slab,
        primitive=primitive,
        max_normal_search=max_normal_search,
    )
    return slabgen.get_slab(shift=shift, tol=tol).as_dict()


def slab_and_vacuum_thickness(slab: Slab) -> tuple[float, float]:
    slab_centered = center_slab(slab.copy())
    frac_z = np.mod(slab_centered.frac_coords[:, 2], 1.0)
    span = float(frac_z.max() - frac_z.min()) if len(frac_z) else 0.0
    c_len = float(np.linalg.norm(slab_centered.lattice.matrix[2]))
    slab_thickness = span * c_len
    vacuum_thickness = max(c_len - slab_thickness, 0.0)
    return slab_thickness, vacuum_thickness


def formula_from_symbols(symbols: Iterable[str]) -> str:
    counts = Counter(symbols)
    if not counts:
        return "-"
    return Composition(dict(counts)).reduced_formula


def layer_tables(
    structure: Structure,
    tolerance_angstrom: float = DEFAULT_LAYER_TOLERANCE,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    if len(structure) == 0:
        empty = pd.DataFrame()
        return empty, empty

    centered = center_slab(structure.copy())
    c_vec = np.asarray(centered.lattice.matrix[2], dtype=float)
    c_len = float(np.linalg.norm(c_vec))
    c_hat = c_vec / c_len if c_len > 0 else np.array([0.0, 0.0, 1.0])

    projections = centered.cart_coords @ c_hat
    order = np.argsort(projections)
    layer_ids = np.zeros(len(centered), dtype=int)

    clusters: list[list[int]] = []
    current_cluster = [int(order[0])]
    previous_value = float(projections[current_cluster[0]])
    for raw_idx in order[1:]:
        idx = int(raw_idx)
        value = float(projections[idx])
        if abs(value - previous_value) > tolerance_angstrom:
            clusters.append(current_cluster)
            current_cluster = [idx]
        else:
            current_cluster.append(idx)
        previous_value = value
    clusters.append(current_cluster)

    for layer_id, members in enumerate(clusters):
        for member in members:
            layer_ids[member] = layer_id

    n_layers = len(clusters)
    site_rows: list[dict] = []
    for site_index, site in enumerate(centered):
        layer_id = int(layer_ids[site_index])
        site_rows.append(
            {
                "site_index": site_index,
                "element": site.specie.symbol,
                "layer_id": layer_id,
                "depth_from_bottom": layer_id,
                "depth_from_top": n_layers - layer_id - 1,
                "x": round(float(centered.cart_coords[site_index, 0]), 4),
                "y": round(float(centered.cart_coords[site_index, 1]), 4),
                "z_cart": round(float(projections[site_index]), 4),
                "z_frac": round(float(centered.frac_coords[site_index, 2]), 4),
            }
        )

    site_df = pd.DataFrame(site_rows).sort_values(
        ["layer_id", "z_cart", "site_index"],
        ignore_index=True,
    )

    layer_rows: list[dict] = []
    for layer_id in range(n_layers):
        members = site_df.loc[site_df["layer_id"] == layer_id]
        layer_rows.append(
            {
                "layer_id": layer_id,
                "depth_from_bottom": layer_id,
                "depth_from_top": n_layers - layer_id - 1,
                "n_sites": int(len(members)),
                "composition": formula_from_symbols(members["element"].tolist()),
                "z_center": round(float(members["z_cart"].mean()), 4),
                "z_min": round(float(members["z_cart"].min()), 4),
                "z_max": round(float(members["z_cart"].max()), 4),
            }
        )

    layer_summary_df = pd.DataFrame(layer_rows)
    return site_df, layer_summary_df


def surface_formula(
    layer_site_df: pd.DataFrame,
    surface_layers: int,
    side: str,
) -> str:
    if layer_site_df.empty:
        return "-"

    if side == "top":
        symbols = layer_site_df.loc[
            layer_site_df["depth_from_top"] < surface_layers,
            "element",
        ].tolist()
    else:
        symbols = layer_site_df.loc[
            layer_site_df["depth_from_bottom"] < surface_layers,
            "element",
        ].tolist()

    return formula_from_symbols(symbols)


def summarize_single_slab(
    slab: Slab,
    termination_label: str | int,
    layer_tolerance: float,
    surface_layers: int,
) -> dict:
    layer_site_df, layer_summary_df = layer_tables(slab, tolerance_angstrom=layer_tolerance)
    slab_thickness, vacuum_thickness = slab_and_vacuum_thickness(slab)
    dipole_vector = np.asarray(slab.dipole, dtype=float)

    return {
        "termination": termination_label,
        "shift": round(float(getattr(slab, "shift", np.nan)), 4),
        "atoms": len(slab),
        "formula": slab.composition.reduced_formula,
        "surface_area(A^2)": round(float(slab.surface_area), 3),
        "atomic_layers~": int(len(layer_summary_df)),
        "slab_thickness(A)": round(float(slab_thickness), 3),
        "vacuum(A)": round(float(vacuum_thickness), 3),
        "polar": bool(slab.is_polar()),
        "symmetric": bool(slab.is_symmetric()),
        "top_surface": surface_formula(layer_site_df, surface_layers=surface_layers, side="top"),
        "bottom_surface": surface_formula(layer_site_df, surface_layers=surface_layers, side="bottom"),
        "dipole_norm": round(float(np.linalg.norm(dipole_vector)), 4),
    }


@cache_data(show_spinner=False)
def summarise_slabs_cached(
    slab_dicts: list[dict],
    layer_tolerance: float,
    surface_layers: int,
) -> pd.DataFrame:
    rows: list[dict] = []
    for index, slab_dict in enumerate(slab_dicts):
        slab = Slab.from_dict(slab_dict)
        rows.append(
            summarize_single_slab(
                slab=slab,
                termination_label=index,
                layer_tolerance=layer_tolerance,
                surface_layers=surface_layers,
            )
        )
    return pd.DataFrame(rows)


def apply_fixed_bottom_layers(
    structure: Structure,
    layer_site_df: pd.DataFrame,
    n_fixed_layers: int,
) -> tuple[Structure, int]:
    export_structure = structure.copy()
    if "selective_dynamics" in export_structure.site_properties:
        export_structure.remove_site_property("selective_dynamics")

    if n_fixed_layers <= 0 or layer_site_df.empty:
        return export_structure, 0

    fixed_indices = set(
        layer_site_df.loc[
            layer_site_df["depth_from_bottom"] < n_fixed_layers,
            "site_index",
        ].tolist()
    )

    selective_dynamics = []
    for site_index in range(len(export_structure)):
        is_fixed = site_index in fixed_indices
        selective_dynamics.append([not is_fixed, not is_fixed, not is_fixed])

    export_structure.add_site_property("selective_dynamics", selective_dynamics)
    return export_structure, len(fixed_indices)


def make_builder_slab(
    slab: Slab,
    rep_a: int,
    rep_b: int,
    orthogonalize_c: bool,
) -> Slab:
    built = slab.copy()
    if orthogonalize_c:
        built = built.get_orthogonal_c_slab()
    built.make_supercell([max(1, rep_a), max(1, rep_b), 1])
    return built


def preview_warning_messages(structure: Structure, show_bonds: bool) -> list[str]:
    messages: list[str] = []
    if len(structure) > ATOM_RENDER_WARNING_THRESHOLD:
        messages.append(
            f"Previewing {len(structure)} atoms. Large 3D scenes may feel sluggish in the browser."
        )
    if show_bonds and len(structure) > BOND_RENDER_THRESHOLD:
        messages.append(
            f"Bond lines are automatically disabled above {BOND_RENDER_THRESHOLD} atoms."
        )
    return messages


def focus_indices_from_mode(
    layer_site_df: pd.DataFrame,
    focus_mode: str,
    layer_depth: int,
    custom_layers: list[int],
) -> set[int] | None:
    if layer_site_df.empty or focus_mode == "All atoms":
        return None

    if focus_mode == "Top surface layers":
        mask = layer_site_df["depth_from_top"] < layer_depth
    elif focus_mode == "Bottom surface layers":
        mask = layer_site_df["depth_from_bottom"] < layer_depth
    else:
        mask = layer_site_df["layer_id"].isin(custom_layers)

    return set(layer_site_df.loc[mask, "site_index"].tolist())


def file_tag_from_miller(miller: tuple[int, int, int]) -> str:
    parts = [str(value).replace("-", "m") for value in miller]
    return "_".join(parts)


def element_color_map(symbols: Iterable[str]) -> dict[str, str]:
    unique_symbols = sorted(set(symbols), key=lambda symbol: Element(symbol).Z)
    return {symbol: PALETTE[index % len(PALETTE)] for index, symbol in enumerate(unique_symbols)}


def element_marker_size(symbol: str, scale: float = 1.0) -> float:
    radius = Element(symbol).atomic_radius
    radius_value = float(radius) if radius is not None else 1.0
    return (8.0 + 6.0 * radius_value) * scale


def build_bond_lines(
    structure: Structure,
    bond_scale: float = 1.15,
    max_bond_length: float = 3.4,
) -> tuple[list[float], list[float], list[float]]:
    if len(structure) > BOND_RENDER_THRESHOLD:
        return [], [], []

    coords = np.asarray(structure.cart_coords)
    symbols = [site.specie.symbol for site in structure]
    radii = [
        float(Element(symbol).atomic_radius) if Element(symbol).atomic_radius is not None else 1.0
        for symbol in symbols
    ]

    x_lines: list[float] = []
    y_lines: list[float] = []
    z_lines: list[float] = []

    for i in range(len(coords)):
        for j in range(i + 1, len(coords)):
            cutoff = min(max_bond_length, bond_scale * (radii[i] + radii[j]))
            dist = float(np.linalg.norm(coords[i] - coords[j]))
            if 0.1 < dist <= cutoff:
                x_lines.extend([coords[i, 0], coords[j, 0], None])
                y_lines.extend([coords[i, 1], coords[j, 1], None])
                z_lines.extend([coords[i, 2], coords[j, 2], None])

    return x_lines, y_lines, z_lines


def cell_edge_traces(structure: Structure) -> list[go.Scatter3d]:
    a, b, c = np.asarray(structure.lattice.matrix)
    corners = np.array(
        [
            [0, 0, 0],
            a,
            b,
            c,
            a + b,
            a + c,
            b + c,
            a + b + c,
        ],
        dtype=float,
    )
    edges = [
        (0, 1),
        (0, 2),
        (0, 3),
        (1, 4),
        (1, 5),
        (2, 4),
        (2, 6),
        (3, 5),
        (3, 6),
        (4, 7),
        (5, 7),
        (6, 7),
    ]

    traces: list[go.Scatter3d] = []
    for start_idx, end_idx in edges:
        traces.append(
            go.Scatter3d(
                x=[corners[start_idx, 0], corners[end_idx, 0]],
                y=[corners[start_idx, 1], corners[end_idx, 1]],
                z=[corners[start_idx, 2], corners[end_idx, 2]],
                mode="lines",
                line=dict(color="rgba(90, 90, 90, 0.60)", width=5),
                hoverinfo="skip",
                showlegend=False,
            )
        )
    return traces


def camera_from_preset(structure: Structure, preset: str, projection_mode: str = "Perspective") -> dict:
    a, b, c = [np.asarray(vector, dtype=float) for vector in structure.lattice.matrix]

    def unit(vector: np.ndarray) -> np.ndarray:
        norm = np.linalg.norm(vector)
        return vector / norm if norm > 0 else vector

    def normal_to_plane(vec1: np.ndarray, vec2: np.ndarray, align_with: np.ndarray) -> np.ndarray:
        normal = np.cross(vec1, vec2)
        if np.linalg.norm(normal) == 0:
            normal = align_with
        if float(np.dot(normal, align_with)) < 0:
            normal = -normal
        return unit(normal)

    if preset == "Top (along +c)":
        eye = 2.4 * unit(c)
    elif preset == "Side (along +a)":
        eye = 2.4 * normal_to_plane(c, b, a)
    elif preset == "Side (along +b)":
        eye = 2.4 * normal_to_plane(c, a, b)
    else:
        eye = 1.6 * unit(a) + 1.6 * unit(b) + 1.3 * unit(c)

    projection_type = "orthographic" if projection_mode == "Orthographic" else "perspective"
    return {
        "eye": {"x": float(eye[0]), "y": float(eye[1]), "z": float(eye[2])},
        "projection": {"type": projection_type},
    }


def structure_figure(
    structure: Structure,
    show_cell: bool,
    show_bonds: bool,
    marker_scale: float,
    camera_preset: str,
    projection_mode: str = "Perspective",
    color_mode: str = "Element",
    layer_site_df: pd.DataFrame | None = None,
    highlighted_indices: set[int] | None = None,
) -> go.Figure:
    fig = go.Figure()

    coords = np.asarray(structure.cart_coords)
    symbols = [site.specie.symbol for site in structure]
    layer_lookup = {}
    if layer_site_df is not None and not layer_site_df.empty:
        layer_lookup = dict(zip(layer_site_df["site_index"], layer_site_df["layer_id"], strict=False))

    effective_show_bonds = show_bonds and len(structure) <= BOND_RENDER_THRESHOLD
    if effective_show_bonds:
        x_lines, y_lines, z_lines = build_bond_lines(structure)
        if x_lines:
            fig.add_trace(
                go.Scatter3d(
                    x=x_lines,
                    y=y_lines,
                    z=z_lines,
                    mode="lines",
                    line=dict(color="rgba(130, 130, 130, 0.40)", width=4),
                    hoverinfo="skip",
                    showlegend=False,
                )
            )

    if highlighted_indices:
        selected_indices = sorted(highlighted_indices)
        if not selected_indices:
            selected_indices = list(range(len(structure)))
        muted_indices = [index for index in range(len(structure)) if index not in highlighted_indices]
        if muted_indices:
            muted_coords = coords[muted_indices]
            muted_hover = [
                f"index={index}<br>element={symbols[index]}<br>x={coords[index, 0]:.3f} A"
                f"<br>y={coords[index, 1]:.3f} A<br>z={coords[index, 2]:.3f} A"
                for index in muted_indices
            ]
            fig.add_trace(
                go.Scatter3d(
                    x=muted_coords[:, 0],
                    y=muted_coords[:, 1],
                    z=muted_coords[:, 2],
                    mode="markers",
                    name="Muted",
                    marker=dict(size=7 * marker_scale, color="rgba(180, 180, 180, 0.35)"),
                    hovertemplate="%{text}<extra></extra>",
                    text=muted_hover,
                    showlegend=False,
                )
            )
    else:
        selected_indices = list(range(len(structure)))

    def hover_text(index: int) -> str:
        layer_text = ""
        if index in layer_lookup:
            layer_text = f"<br>layer={layer_lookup[index]}"
        return (
            f"index={index}<br>element={symbols[index]}{layer_text}"
            f"<br>x={coords[index, 0]:.3f} A"
            f"<br>y={coords[index, 1]:.3f} A"
            f"<br>z={coords[index, 2]:.3f} A"
        )

    if color_mode == "Layer" and layer_lookup:
        unique_layers = sorted({layer_lookup.get(index, -1) for index in selected_indices})
        for layer_id in unique_layers:
            layer_indices = [index for index in selected_indices if layer_lookup.get(index, -1) == layer_id]
            if not layer_indices:
                continue
            subset = coords[layer_indices]
            fig.add_trace(
                go.Scatter3d(
                    x=subset[:, 0],
                    y=subset[:, 1],
                    z=subset[:, 2],
                    mode="markers",
                    name=f"Layer {layer_id}",
                    marker=dict(
                        size=10 * marker_scale,
                        color=LAYER_PALETTE[layer_id % len(LAYER_PALETTE)],
                        opacity=0.96,
                        line=dict(width=0.5, color="rgba(30, 30, 30, 0.65)"),
                    ),
                    hovertemplate="%{text}<extra></extra>",
                    text=[hover_text(index) for index in layer_indices],
                )
            )
    else:
        colors = element_color_map(symbols)
        for symbol in sorted(set(symbols), key=lambda value: Element(value).Z):
            element_indices = [index for index in selected_indices if symbols[index] == symbol]
            if not element_indices:
                continue
            subset = coords[element_indices]
            fig.add_trace(
                go.Scatter3d(
                    x=subset[:, 0],
                    y=subset[:, 1],
                    z=subset[:, 2],
                    mode="markers",
                    name=symbol,
                    marker=dict(
                        size=element_marker_size(symbol, scale=marker_scale),
                        color=colors[symbol],
                        opacity=0.96,
                        line=dict(width=0.5, color="rgba(40, 40, 40, 0.70)"),
                    ),
                    hovertemplate="%{text}<extra></extra>",
                    text=[hover_text(index) for index in element_indices],
                )
            )

    if show_cell:
        for trace in cell_edge_traces(structure):
            fig.add_trace(trace)

    fig.update_layout(
        margin=dict(l=0, r=0, b=0, t=20),
        legend=dict(itemsizing="constant"),
        scene=dict(
            xaxis=dict(showbackground=False, showgrid=False, zeroline=False, title="x (A)"),
            yaxis=dict(showbackground=False, showgrid=False, zeroline=False, title="y (A)"),
            zaxis=dict(showbackground=False, showgrid=False, zeroline=False, title="z (A)"),
            aspectmode="data",
            camera=camera_from_preset(structure, camera_preset, projection_mode=projection_mode),
        ),
    )
    return fig


def layer_profile_figure(layer_summary_df: pd.DataFrame) -> go.Figure:
    fig = go.Figure()
    if layer_summary_df.empty:
        fig.update_layout(
            margin=dict(l=0, r=0, b=0, t=20),
            xaxis_title="Atoms",
            yaxis_title="Layer",
        )
        return fig

    hover_text = [
        f"Layer {int(row.layer_id)}<br>composition={row.composition}<br>z_center={row.z_center:.3f} A"
        for row in layer_summary_df.itertuples()
    ]
    fig.add_trace(
        go.Bar(
            x=layer_summary_df["n_sites"],
            y=[f"Layer {layer_id}" for layer_id in layer_summary_df["layer_id"]],
            orientation="h",
            marker_color=[
                LAYER_PALETTE[int(layer_id) % len(LAYER_PALETTE)]
                for layer_id in layer_summary_df["layer_id"]
            ],
            text=layer_summary_df["composition"],
            textposition="outside",
            hovertemplate="%{customdata}<extra></extra>",
            customdata=hover_text,
        )
    )
    fig.update_layout(
        margin=dict(l=0, r=0, b=0, t=20),
        xaxis_title="Atoms in layer",
        yaxis_title="Layer",
        yaxis=dict(autorange="reversed"),
    )
    return fig


def run_app() -> None:

    st.title("Slab Studio")
    st.caption(
        "Interactive slab builder for facet browsing, termination comparison, "
        "layer-aware surface inspection, and export-ready slab generation."
    )
    
    with st.sidebar:
        st.header("1) Structure")
        source_mode = st.radio("Source", ["Demo", "Upload"], horizontal=True)
    
        if source_mode == "Demo":
            demo_name = st.selectbox(
                "Demo structure",
                ["Si (diamond)", "NaCl (rocksalt)", "SrTiO3 (perovskite)", "TiO2 (rutile)"],
                index=0,
            )
            structure_raw = build_demo_structure(demo_name)
        else:
            uploaded_file = st.file_uploader(
                "Upload structure file",
                help="CIF, POSCAR/CONTCAR, XSF, CSSR, JSON, YAML, RES, or MCSQS supported by pymatgen.",
            )
            parser_hint = st.selectbox(
                "Parser",
                ["auto", "cif", "poscar", "xsf", "cssr", "json", "yaml", "yml", "res", "mcsqs"],
                index=0,
            )
            if uploaded_file is None:
                st.info("Upload a structure file or switch back to Demo mode.")
                st.stop()
            try:
                structure_raw = load_structure_from_upload(uploaded_file, parser_hint)
            except Exception as exc:  # pragma: no cover - UI path
                st.error(f"Could not parse the structure file: {exc}")
                st.stop()
    
        use_conventional_cell = st.checkbox("Use conventional standard cell", value=True)
        symprec = st.number_input(
            "symprec",
            min_value=1e-4,
            max_value=1e-1,
            value=1e-2,
            step=1e-3,
            format="%.4f",
        )
    
        try:
            normalized_dict = normalize_structure_cached(
                structure_dict=structure_raw.as_dict(),
                use_conventional_cell=use_conventional_cell,
                symprec=symprec,
            )
            structure = Structure.from_dict(normalized_dict)
        except Exception as exc:  # pragma: no cover - UI path
            st.error(f"Could not normalize the structure: {exc}")
            st.stop()
    
        st.header("2) Facet Explorer")
        facet_mode = st.radio("Facet input", ["Distinct list", "Manual"], horizontal=True)
        facet_records: list[dict] = []
    
        if facet_mode == "Distinct list":
            max_index = st.slider("Max |hkl|", min_value=1, max_value=4, value=2)
            try:
                facet_records = facet_records_cached(structure_dict=structure.as_dict(), max_index=max_index)
            except Exception as exc:  # pragma: no cover - UI path
                st.error(f"Could not enumerate facet candidates: {exc}")
                st.stop()
    
            if not facet_records:
                st.error("No facet candidates were found for this structure.")
                st.stop()
    
            facet_options = [tuple(record["facet"]) for record in facet_records]
            selected_facet_idx = st.selectbox(
                "Facet",
                options=list(range(len(facet_options))),
                format_func=lambda index: (
                    f"{format_miller(facet_options[index])} | "
                    f"d={facet_records[index]['d_hkl(A)']:.3f} A | "
                    f"eq={facet_records[index]['equivalent_facets']}"
                ),
            )
            miller = facet_options[int(selected_facet_idx)]
        else:
            cols = st.columns(3)
            h = cols[0].number_input("h", min_value=-6, max_value=6, value=1, step=1)
            k = cols[1].number_input("k", min_value=-6, max_value=6, value=0, step=1)
            l = cols[2].number_input("l", min_value=-6, max_value=6, value=0, step=1)
            try:
                miller = sanitize_miller(h, k, l)
            except ValueError as exc:  # pragma: no cover - UI path
                st.error(str(exc))
                st.stop()
    
        st.header("3) Slab Builder")
        thickness_mode = st.radio("Thickness mode", ["hkl planes", "Angstrom"], horizontal=True)
        if thickness_mode == "hkl planes":
            slab_size_value = st.slider("Target hkl plane count", min_value=1, max_value=24, value=6)
        else:
            slab_size_value = st.number_input(
                "Minimum slab thickness (A)",
                min_value=1.0,
                max_value=80.0,
                value=12.0,
                step=1.0,
            )
        vacuum_value = st.number_input(
            "Target vacuum (A)",
            min_value=2.0,
            max_value=80.0,
            value=15.0,
            step=1.0,
        )
        termination_mode = st.radio(
            "Termination mode",
            ["Generated terminations", "Custom shift"],
            horizontal=True,
        )
        custom_shift = st.slider(
            "Custom termination shift",
            min_value=0.0,
            max_value=0.99,
            value=0.50,
            step=0.01,
            disabled=termination_mode != "Custom shift",
        )
    
        with st.expander("Advanced generator options", expanded=False):
            primitive = st.checkbox("Primitive reduction", value=False)
            center_the_slab = st.checkbox("Center slab in vacuum", value=True)
            lll_reduce = st.checkbox("LLL reduce lattice", value=False)
            symmetrize = st.checkbox("Force symmetric slab when possible", value=False)
            filter_out_sym_slabs = st.checkbox("Remove duplicate symmetric terminations", value=True)
            max_normal_search = st.slider(
                "max_normal_search",
                min_value=1,
                max_value=10,
                value=max(1, max(abs(index) for index in miller)),
            )
            ftol = st.number_input(
                "ftol (plane clustering, A)",
                min_value=0.0,
                max_value=1.0,
                value=0.10,
                step=0.05,
                format="%.2f",
            )
            tol = st.number_input(
                "tol (matching / primitive reduction)",
                min_value=0.0,
                max_value=1.0,
                value=0.10,
                step=0.05,
                format="%.2f",
            )
    
        st.header("4) Preview + Export")
        rep_a = st.slider("Repeat along a", min_value=1, max_value=4, value=2)
        rep_b = st.slider("Repeat along b", min_value=1, max_value=4, value=2)
        orthogonalize_c = st.checkbox("Orthogonalize c before repeat/export", value=False)
        marker_scale = st.slider("Atom marker scale", min_value=0.5, max_value=2.0, value=1.0, step=0.1)
        camera_preset = st.selectbox(
            "Camera preset",
            ["Isometric", "Top (along +c)", "Side (along +a)", "Side (along +b)"],
            index=0,
        )
        show_cell = st.checkbox("Show unit cell", value=True)
        show_bonds = st.checkbox("Show heuristic bond lines", value=True)
        layer_tolerance = st.slider(
            "Layer clustering tolerance (A)",
            min_value=0.10,
            max_value=1.00,
            value=DEFAULT_LAYER_TOLERANCE,
            step=0.05,
        )
        surface_probe_layers = st.slider(
            "Surface depth for summary (layers)",
            min_value=1,
            max_value=4,
            value=1,
        )
    
    st.caption(
        "Preview and exported slabs share the same repeat and orthogonalization settings, "
        "so the downloaded structure matches the slab you inspect on screen."
    )
    
    info_cols = st.columns([1.1, 0.9, 1.2])
    try:
        sg_symbol, sg_number = structure.get_space_group_info(symprec=symprec)
        sg_text = f"{sg_symbol} (No. {sg_number})"
    except Exception:
        sg_text = "N/A"
    
    info_cols[0].metric("Formula", structure.composition.reduced_formula)
    info_cols[1].metric("Sites", len(structure))
    info_cols[2].metric("Space group", sg_text)
    
    st.write(
        {
            "lattice_abc(A)": tuple(round(value, 4) for value in structure.lattice.abc),
            "angles(deg)": tuple(round(value, 3) for value in structure.lattice.angles),
            "selected_facet": format_miller(miller),
        }
    )
    
    if facet_mode == "Distinct list" and facet_records:
        facet_df = pd.DataFrame(facet_records)
        facet_df["selected"] = facet_df["facet"].apply(tuple) == miller
        facet_df = facet_df[["selected", "facet_label", "d_hkl(A)", "equivalent_facets", "family"]]
        with st.expander("Facet explorer table", expanded=False):
            st.dataframe(facet_df, width="stretch", height=220)
    
    try:
        equivalent_facets = get_symmetrically_equivalent_miller_indices(structure, miller, return_hkil=False)
        st.caption("Equivalent facets: " + ", ".join(format_miller(index) for index in equivalent_facets))
    except Exception:
        pass
    
    try:
        min_slab_size, min_vacuum_size, in_unit_planes, thickness_meta = slab_generation_kwargs(
            structure=structure,
            miller=miller,
            thickness_mode=thickness_mode,
            slab_size_value=slab_size_value,
            vacuum_value=vacuum_value,
        )
    except Exception as exc:  # pragma: no cover - UI path
        st.error(f"Could not compute slab builder parameters: {exc}")
        st.stop()
    
    st.caption(
        " | ".join(
            [
                f"d_hkl = {thickness_meta['d_hkl']:.4f} A",
                f"generator min_slab_size = {thickness_meta['min_slab_size']}",
                f"generator min_vacuum_size = {thickness_meta['min_vacuum_size']}",
                f"in_unit_planes = {thickness_meta['in_unit_planes']}",
            ]
        )
    )
    
    with st.spinner("Generating slab terminations..."):
        try:
            slab_dicts = generate_slabs_cached(
                structure_dict=structure.as_dict(),
                miller=miller,
                min_slab_size=min_slab_size,
                min_vacuum_size=min_vacuum_size,
                in_unit_planes=in_unit_planes,
                lll_reduce=lll_reduce,
                center_the_slab=center_the_slab,
                primitive=primitive,
                max_normal_search=max_normal_search,
                symmetrize=symmetrize,
                ftol=ftol,
                tol=tol,
                filter_out_sym_slabs=filter_out_sym_slabs,
            )
            slabs = [Slab.from_dict(slab_dict) for slab_dict in slab_dicts]
        except Exception as exc:  # pragma: no cover - UI path
            st.error(f"Could not generate slabs: {exc}")
            st.stop()
    
    summary_df = (
        summarise_slabs_cached(
            slab_dicts=slab_dicts,
            layer_tolerance=layer_tolerance,
            surface_layers=surface_probe_layers,
        )
        if slab_dicts
        else pd.DataFrame()
    )
    
    custom_slab = None
    if termination_mode == "Custom shift":
        try:
            custom_slab = Slab.from_dict(
                generate_single_slab_cached(
                    structure_dict=structure.as_dict(),
                    miller=miller,
                    min_slab_size=min_slab_size,
                    min_vacuum_size=min_vacuum_size,
                    in_unit_planes=in_unit_planes,
                    lll_reduce=lll_reduce,
                    center_the_slab=center_the_slab,
                    primitive=primitive,
                    max_normal_search=max_normal_search,
                    shift=custom_shift,
                    tol=tol,
                )
            )
        except Exception as exc:  # pragma: no cover - UI path
            st.error(f"Could not build custom-shift slab: {exc}")
            st.stop()
    
    
    if termination_mode == "Generated terminations" and not slabs:
        st.warning(
            "No generated terminations were found. Try adjusting symmetrize, ftol, tol, "
            "or switch to Custom shift mode."
        )
        st.stop()
    
    generated_terminations = summary_df["termination"].tolist() if not summary_df.empty else []
    generated_state_key = "selected_generated_termination"
    if generated_terminations and (
        generated_state_key not in st.session_state
        or st.session_state[generated_state_key] not in generated_terminations
    ):
        st.session_state[generated_state_key] = int(generated_terminations[0])
    
    browser_col1, browser_col2 = st.columns([0.85, 1.15])
    with browser_col1:
        st.subheader("Termination browser")
        compare_enabled = False
        compare_termination = None
    
        if generated_terminations:
            current_generated = int(st.session_state[generated_state_key])
            current_position = generated_terminations.index(current_generated)
            nav_cols = st.columns(3)
            if nav_cols[0].button("Prev", disabled=current_position == 0):
                st.session_state[generated_state_key] = int(generated_terminations[current_position - 1])
            if nav_cols[1].button("Next", disabled=current_position == len(generated_terminations) - 1):
                st.session_state[generated_state_key] = int(generated_terminations[current_position + 1])
            st.select_slider(
                "Generated termination",
                options=generated_terminations,
                key=generated_state_key,
                format_func=lambda index: (
                    f"#{index} | shift="
                    f"{summary_df.loc[summary_df['termination'] == index, 'shift'].iloc[0]:.4f}"
                ),
            )
            current_generated = int(st.session_state[generated_state_key])
        else:
            current_generated = None
    
        if termination_mode == "Custom shift":
            st.info(
                "Custom shift mode uses SlabGenerator.get_slab(shift) directly, which is useful for "
                "interactive surface tuning and may not match the filtered generated termination list."
            )
            if not summary_df.empty:
                nearest_row = summary_df.iloc[(summary_df["shift"] - custom_shift).abs().argmin()]
                st.caption(
                    f"Nearest generated reference: #{int(nearest_row['termination'])} "
                    f"at shift={nearest_row['shift']:.4f}"
                )
    
        compare_candidates: list[int] = []
        if termination_mode == "Generated terminations" and len(generated_terminations) > 1 and current_generated is not None:
            compare_candidates = [value for value in generated_terminations if value != current_generated]
        elif termination_mode == "Custom shift" and generated_terminations:
            compare_candidates = generated_terminations
    
        compare_enabled = st.checkbox(
            "Enable comparison panel",
            value=bool(compare_candidates) and termination_mode == "Custom shift",
            disabled=not compare_candidates,
        )
        if compare_enabled and compare_candidates:
            compare_termination = int(
                st.selectbox(
                    "Compare against",
                    options=compare_candidates,
                    format_func=lambda index: (
                        f"#{index} | shift="
                        f"{summary_df.loc[summary_df['termination'] == index, 'shift'].iloc[0]:.4f}"
                    ),
                )
            )
    
    with browser_col2:
        if not summary_df.empty:
            st.dataframe(
                summary_df[
                    [
                        "termination",
                        "shift",
                        "atoms",
                        "atomic_layers~",
                        "top_surface",
                        "bottom_surface",
                        "polar",
                        "symmetric",
                    ]
                ],
                width="stretch",
                height=280,
            )
        else:
            st.info("Generated termination list is empty. Custom shift preview is still available.")
    
    if termination_mode == "Generated terminations":
        active_base_label = f"Generated #{int(st.session_state[generated_state_key])}"
        active_base_slab = slabs[int(st.session_state[generated_state_key])]
    else:
        active_base_label = f"Custom shift {custom_shift:.3f}"
        active_base_slab = custom_slab
    
    active_preview_slab = make_builder_slab(
        slab=active_base_slab,
        rep_a=rep_a,
        rep_b=rep_b,
        orthogonalize_c=orthogonalize_c,
    )
    active_preview_meta = summarize_single_slab(
        slab=active_preview_slab,
        termination_label=active_base_label,
        layer_tolerance=layer_tolerance,
        surface_layers=surface_probe_layers,
    )
    oriented_bulk_preview = getattr(active_base_slab, "oriented_unit_cell", structure)
    
    compare_preview_slab = None
    compare_preview_meta = None
    compare_label = None
    if compare_enabled and compare_termination is not None:
        compare_label = f"Generated #{compare_termination}"
        compare_preview_slab = make_builder_slab(
            slab=slabs[int(compare_termination)],
            rep_a=rep_a,
            rep_b=rep_b,
            orthogonalize_c=orthogonalize_c,
        )
        compare_preview_meta = summarize_single_slab(
            slab=compare_preview_slab,
            termination_label=compare_label,
            layer_tolerance=layer_tolerance,
            surface_layers=surface_probe_layers,
        )
    
    builder_tab, layer_tab, export_tab = st.tabs(["Builder", "Layer inspector", "Export"])
    
    with builder_tab:
        builder_metric_cols = st.columns(7)
        builder_metric_cols[0].metric("Active slab", active_base_label)
        builder_metric_cols[1].metric("Shift", f"{active_preview_meta['shift']:.4f}")
        builder_metric_cols[2].metric("Atoms", int(active_preview_meta["atoms"]))
        builder_metric_cols[3].metric("Layers~", int(active_preview_meta["atomic_layers~"]))
        builder_metric_cols[4].metric("Top surface", str(active_preview_meta["top_surface"]))
        builder_metric_cols[5].metric("Bottom surface", str(active_preview_meta["bottom_surface"]))
        builder_metric_cols[6].metric("Vacuum", f"{active_preview_meta['vacuum(A)']:.2f} A")
    
        preview_cols = st.columns(2)
        with preview_cols[0]:
            st.markdown("**Oriented bulk preview**")
            bulk_fig = structure_figure(
                structure=oriented_bulk_preview,
                show_cell=show_cell,
                show_bonds=show_bonds,
                marker_scale=marker_scale,
                camera_preset=camera_preset,
            )
            st.plotly_chart(bulk_fig, width="stretch")
    
        with preview_cols[1]:
            st.markdown(f"**Active slab preview: {active_base_label}**")
            active_fig = structure_figure(
                structure=active_preview_slab,
                show_cell=show_cell,
                show_bonds=show_bonds,
                marker_scale=marker_scale,
                camera_preset=camera_preset,
            )
            st.plotly_chart(active_fig, width="stretch")
            for message in preview_warning_messages(active_preview_slab, show_bonds):
                st.warning(message)
    
        if compare_preview_slab is not None and compare_preview_meta is not None and compare_label is not None:
            st.markdown("**Termination comparison**")
            compare_cols = st.columns(2)
            with compare_cols[0]:
                st.caption(active_base_label)
                st.plotly_chart(
                    structure_figure(
                        structure=active_preview_slab,
                        show_cell=show_cell,
                        show_bonds=show_bonds,
                        marker_scale=marker_scale,
                        camera_preset=camera_preset,
                    ),
                    width="stretch",
                )
            with compare_cols[1]:
                st.caption(compare_label)
                st.plotly_chart(
                    structure_figure(
                        structure=compare_preview_slab,
                        show_cell=show_cell,
                        show_bonds=show_bonds,
                        marker_scale=marker_scale,
                        camera_preset=camera_preset,
                    ),
                    width="stretch",
                )
    
            compare_metrics = pd.DataFrame(
                [
                    {"slab": active_base_label, **active_preview_meta},
                    {"slab": compare_label, **compare_preview_meta},
                ]
            )
            st.dataframe(
                compare_metrics[
                    [
                        "slab",
                        "shift",
                        "atoms",
                        "atomic_layers~",
                        "surface_area(A^2)",
                        "top_surface",
                        "bottom_surface",
                        "polar",
                        "symmetric",
                    ]
                ],
                width="stretch",
                height=140,
            )
    
    with layer_tab:
        active_layer_site_df, active_layer_summary_df = layer_tables(
            active_preview_slab,
            tolerance_angstrom=layer_tolerance,
        )
        layer_count = int(len(active_layer_summary_df))
    
        if active_layer_summary_df.empty:
            st.info("Layer analysis is not available for this slab.")
            fixed_bottom_layers = 0
            export_slab = active_preview_slab.copy()
            fixed_atom_count = 0
        else:
            inspector_cols = st.columns([0.7, 1.3])
            with inspector_cols[0]:
                color_mode = st.radio("Color by", ["Element", "Layer"], horizontal=True)
                focus_mode = st.selectbox(
                    "Highlight",
                    ["All atoms", "Top surface layers", "Bottom surface layers", "Custom layer ids"],
                )
                focus_depth = st.slider(
                    "Highlighted layer depth",
                    min_value=1,
                    max_value=layer_count,
                    value=min(surface_probe_layers, layer_count),
                    disabled=focus_mode in {"All atoms", "Custom layer ids"},
                )
                custom_layer_ids = st.multiselect(
                    "Custom layers",
                    options=active_layer_summary_df["layer_id"].tolist(),
                    default=[int(active_layer_summary_df["layer_id"].iloc[-1])],
                    disabled=focus_mode != "Custom layer ids",
                )
                fixed_bottom_layers = st.slider(
                    "Fix bottom layers in export",
                    min_value=0,
                    max_value=layer_count,
                    value=0,
                )
    
                highlighted_indices = focus_indices_from_mode(
                    layer_site_df=active_layer_site_df,
                    focus_mode=focus_mode,
                    layer_depth=focus_depth,
                    custom_layers=custom_layer_ids,
                )
                export_slab, fixed_atom_count = apply_fixed_bottom_layers(
                    structure=active_preview_slab,
                    layer_site_df=active_layer_site_df,
                    n_fixed_layers=fixed_bottom_layers,
                )
    
                surface_cols = st.columns(3)
                surface_cols[0].metric("Top surface", surface_formula(active_layer_site_df, surface_probe_layers, "top"))
                surface_cols[1].metric(
                    "Bottom surface",
                    surface_formula(active_layer_site_df, surface_probe_layers, "bottom"),
                )
                surface_cols[2].metric("Fixed atoms", fixed_atom_count)
    
                st.dataframe(
                    active_layer_summary_df[
                        [
                            "layer_id",
                            "n_sites",
                            "composition",
                            "z_center",
                            "z_min",
                            "z_max",
                        ]
                    ],
                    width="stretch",
                    height=220,
                )
                st.plotly_chart(layer_profile_figure(active_layer_summary_df), width="stretch")
    
            with inspector_cols[1]:
                inspector_fig = structure_figure(
                    structure=active_preview_slab,
                    show_cell=show_cell,
                    show_bonds=show_bonds,
                    marker_scale=marker_scale,
                    camera_preset=camera_preset,
                    color_mode=color_mode,
                    layer_site_df=active_layer_site_df,
                    highlighted_indices=highlighted_indices,
                )
                st.plotly_chart(inspector_fig, width="stretch")
                st.dataframe(
                    active_layer_site_df[
                        [
                            "site_index",
                            "element",
                            "layer_id",
                            "depth_from_bottom",
                            "depth_from_top",
                            "x",
                            "y",
                            "z_cart",
                            "z_frac",
                        ]
                    ],
                    width="stretch",
                    height=360,
                )
    
    with export_tab:
        if "export_slab" not in locals():
            export_slab = active_preview_slab.copy()
            fixed_bottom_layers = 0
            fixed_atom_count = 0
    
        export_tag = (
            f"term{int(st.session_state[generated_state_key])}"
            if termination_mode == "Generated terminations"
            else f"shift{custom_shift:.3f}".replace(".", "p")
        )
        root_name = (
            f"slab_{file_tag_from_miller(miller)}_{export_tag}_"
            f"a{rep_a}_b{rep_b}"
            f"{'_ortho' if orthogonalize_c else ''}"
        )
    
        export_cols = st.columns(3)
        export_cols[0].metric("Export atoms", len(export_slab))
        export_cols[1].metric("Fixed bottom layers", fixed_bottom_layers)
        export_cols[2].metric("Fixed atoms", fixed_atom_count)
    
        export_summary = pd.DataFrame(
            [
                {"setting": "Facet", "value": format_miller(miller)},
                {"setting": "Active slab", "value": active_base_label},
                {"setting": "Repeat", "value": f"{rep_a} x {rep_b} x 1"},
                {"setting": "Orthogonalized c", "value": "Yes" if orthogonalize_c else "No"},
                {"setting": "Selective dynamics", "value": f"Bottom {fixed_bottom_layers} layers fixed"},
            ]
        )
        st.dataframe(export_summary.astype(str), width="stretch")
    
        current_cif = active_preview_slab.to(fmt="cif")
        current_poscar = str(Poscar(active_preview_slab))
        fixed_poscar = str(Poscar(export_slab))
    
        download_cols = st.columns(3)
        with download_cols[0]:
            st.download_button(
                label="Download current slab (.cif)",
                data=current_cif,
                file_name=f"{root_name}.cif",
                mime="chemical/x-cif",
            )
        with download_cols[1]:
            st.download_button(
                label="Download current slab (POSCAR)",
                data=current_poscar,
                file_name=f"POSCAR_{root_name}",
                mime="text/plain",
            )
        with download_cols[2]:
            st.download_button(
                label="Download slab + fixed layers (POSCAR)",
                data=fixed_poscar,
                file_name=f"POSCAR_{root_name}_fix{fixed_bottom_layers}",
                mime="text/plain",
                disabled=fixed_bottom_layers == 0,
            )
    
        st.markdown(
            """
            Builder notes:
            - `Generated terminations` uses the filtered list from `SlabGenerator.get_slabs(...)`.
            - `Custom shift` is a direct `get_slab(shift)` workflow for fast interactive surface tuning.
            - Layer counts, slab thickness, and vacuum thickness are practical geometric estimates.
            """
        )

if RUNNING_IN_STREAMLIT:
    run_app()
elif __name__ == "__main__":
    print("This app must be launched with: streamlit run slab_studio.py")

