from __future__ import annotations

import html
import os
import re
import sys
import tempfile
import time
from pathlib import Path

import numpy as np
import pandas as pd
from PyQt5 import uic
from PyQt5.QtCore import Qt, QTimer, QUrl
from PyQt5.QtWidgets import (
    QApplication,
    QComboBox,
    QDoubleSpinBox,
    QFileDialog,
    QLabel,
    QMainWindow,
    QMessageBox,
    QPlainTextEdit,
    QSizePolicy,
    QSpinBox,
    QTableWidget,
    QTableWidgetItem,
    QTextBrowser,
    QWidget,
)
from pymatgen.core import Structure
from pymatgen.core.surface import Slab
from pymatgen.io.vasp import Poscar
from plotly.offline import get_plotlyjs

from slab_studio import (
    DEFAULT_LAYER_TOLERANCE,
    apply_fixed_bottom_layers,
    build_slab_generator,
    build_demo_structure,
    file_tag_from_miller,
    format_miller,
    generate_single_slab_cached,
    generate_slabs_cached,
    get_facet_candidates,
    layer_tables,
    make_builder_slab,
    normalize_for_surface,
    preview_warning_messages,
    slab_generation_kwargs,
    structure_figure,
    summarise_slabs_cached,
    summarize_single_slab,
)

try:
    from PyQt5.QtWebEngineWidgets import QWebEngineView
except Exception:
    QWebEngineView = None


BASE_DIR = Path(__file__).resolve().parent
UI_PATH = BASE_DIR / "slab_studio_qt.ui"

DEMO_NAMES = [
    "Si (diamond)",
    "NaCl (rocksalt)",
    "SrTiO3 (perovskite)",
    "TiO2 (rutile)",
]
PARSER_OPTIONS = ["auto", "cif", "poscar", "xsf", "cssr", "json", "yaml", "yml", "res", "mcsqs"]
CAMERA_PRESETS = ["Isometric", "Top (along +c)", "Side (along +a)", "Side (along +b)"]
PROJECTION_MODES = ["Perspective", "Orthographic"]


def load_structure_from_path(path: str, parser_hint: str) -> Structure:
    file_path = Path(path)
    if parser_hint == "auto":
        return Structure.from_file(file_path)
    text = file_path.read_text(encoding="utf-8", errors="ignore")
    return Structure.from_str(text, fmt=parser_hint)


def configure_spinbox(widget: QSpinBox, minimum: int, maximum: int, value: int, step: int = 1) -> None:
    widget.setRange(minimum, maximum)
    widget.setSingleStep(step)
    widget.setValue(value)


def configure_double_spinbox(
    widget: QDoubleSpinBox,
    minimum: float,
    maximum: float,
    value: float,
    step: float,
    decimals: int = 2,
) -> None:
    widget.setRange(minimum, maximum)
    widget.setDecimals(decimals)
    widget.setSingleStep(step)
    widget.setValue(value)


def sanitize_ui_for_pyqt5(ui_text: str) -> str:
    """Remove Designer-only XML fragments that PyQt5.uic cannot load."""
    sanitized = re.sub(
        r'<property name="contentsMargins">\s*'
        r'<left>\s*\d+\s*</left>\s*'
        r'<top>\s*\d+\s*</top>\s*'
        r'<right>\s*\d+\s*</right>\s*'
        r'<bottom>\s*\d+\s*</bottom>\s*'
        r'</property>\s*',
        "",
        ui_text,
        flags=re.DOTALL,
    )
    return sanitized


def sanitize_plotly_javascript(plotly_js: str) -> str:
    """Downgrade CSS selectors that Qt WebEngine 5 cannot parse inside Plotly's injected styles."""
    return plotly_js.replace(":focus-visible", ":focus")


class SlabStudioQtWindow(QMainWindow):
    def __init__(self) -> None:
        super().__init__()
        self._load_ui()

        self.structure_raw: Structure | None = None
        self.structure: Structure | None = None
        self.facet_candidates: list[tuple[int, int, int]] = []
        self.generated_slab_dicts: list[dict] = []
        self.generated_slabs: list[Slab] = []
        self.summary_df = pd.DataFrame()
        self.active_base_slab: Slab | None = None
        self.active_preview_slab: Slab | None = None
        self.active_export_slab: Structure | None = None
        self.active_layer_site_df = pd.DataFrame()
        self.active_layer_summary_df = pd.DataFrame()
        self.active_meta: dict | None = None
        self.active_fixed_atoms = 0
        self.last_preview_html: Path | None = None
        self.preview_webview: QWebEngineView | None = None
        self.preview_fallback: QTextBrowser | None = None
        self.custom_shift_options: list[dict[str, float | int | str]] = []
        self.auto_load_timer = QTimer(self)
        self.auto_load_timer.setSingleShot(True)
        self.auto_load_timer.timeout.connect(self._auto_load_current_source)

        self.metric_labels = {
            "Formula": self.formulaValueLabel,
            "Sites": self.sitesValueLabel,
            "Space group": self.spaceGroupValueLabel,
            "Facet": self.facetValueLabel,
            "Active slab": self.activeSlabValueLabel,
            "Shift": self.shiftValueLabel,
            "Atoms": self.atomsValueLabel,
            "Layers~": self.layersValueLabel,
            "Top surface": self.topSurfaceValueLabel,
            "Bottom surface": self.bottomSurfaceValueLabel,
            "Vacuum (A)": self.vacuumValueLabel,
            "Slab thickness (A)": self.slabThicknessValueLabel,
        }

        self._configure_widgets()
        self._configure_tables()
        self._configure_preview_host()
        self._configure_window_layout()
        self._connect_signals()
        self._sync_source_mode()
        self._sync_facet_mode()
        self._sync_thickness_mode()
        self._sync_termination_mode()
        QTimer.singleShot(0, self.load_structure)

    def _load_ui(self) -> None:
        try:
            uic.loadUi(str(UI_PATH), self)
            return
        except Exception as exc:
            try:
                ui_text = UI_PATH.read_text(encoding="utf-8")
            except Exception:
                raise

            sanitized_text = sanitize_ui_for_pyqt5(ui_text)
            if sanitized_text == ui_text:
                raise

            compat_dir = Path(tempfile.gettempdir()) / "slab_studio_qt_ui_compat"
            compat_dir.mkdir(parents=True, exist_ok=True)
            compat_path = compat_dir / UI_PATH.name
            compat_path.write_text(sanitized_text, encoding="utf-8")

            try:
                uic.loadUi(str(compat_path), self)
                return
            except Exception as compat_exc:
                raise RuntimeError(
                    "Failed to load the Qt Designer UI. "
                    f"Original error: {exc}. Compatibility retry error: {compat_exc}."
                ) from compat_exc

    def _configure_widgets(self) -> None:
        self.sourceModeCombo.addItems(["Demo", "Upload"])
        self.demoCombo.addItems(DEMO_NAMES)
        self.parserCombo.addItems(PARSER_OPTIONS)
        self.facetModeCombo.addItems(["Distinct list", "Manual"])
        self.thicknessModeCombo.addItems(["hkl planes", "Angstrom"])
        self.terminationModeCombo.addItems(["Generated terminations", "Custom shift"])
        self.cameraCombo.addItems(CAMERA_PRESETS)

        self.filePathEdit.setPlaceholderText("Choose a CIF/POSCAR/XSF/... file")
        self.useConventionalCheckBox.setChecked(True)
        self.centerCheckBox.setChecked(True)
        self.filterSymCheckBox.setChecked(True)
        self.showCellCheckBox.setChecked(True)
        self.showBondsCheckBox.setChecked(True)
        self.openPreviewButton.setText("Refresh Preview")

        configure_double_spinbox(self.symprecSpin, 1e-4, 1e-1, 1e-2, 1e-3, decimals=4)
        configure_spinbox(self.maxIndexSpin, 1, 4, 2)
        for widget, value in ((self.hSpin, 1), (self.kSpin, 0), (self.lSpin, 0)):
            configure_spinbox(widget, -6, 6, value)
        configure_spinbox(self.planeCountSpin, 1, 24, 6)
        configure_double_spinbox(self.slabThicknessSpin, 1.0, 80.0, 12.0, 1.0)
        configure_double_spinbox(self.vacuumSpin, 2.0, 80.0, 15.0, 1.0)
        configure_double_spinbox(self.customShiftSpin, 0.0, 0.9999, 0.5000, 0.0005, decimals=4)
        configure_spinbox(self.maxNormalSearchSpin, 1, 10, 1)
        configure_double_spinbox(self.ftolSpin, 0.0, 1.0, 0.10, 0.05)
        configure_double_spinbox(self.tolSpin, 0.0, 1.0, 0.10, 0.05)
        configure_spinbox(self.repASpin, 1, 4, 2)
        configure_spinbox(self.repBSpin, 1, 4, 2)
        configure_double_spinbox(self.markerScaleSpin, 0.5, 2.0, 1.0, 0.1, decimals=1)
        configure_double_spinbox(self.layerToleranceSpin, 0.10, 1.00, DEFAULT_LAYER_TOLERANCE, 0.05)
        configure_spinbox(self.surfaceDepthSpin, 1, 4, 1)
        configure_spinbox(self.fixedBottomLayersSpin, 0, 64, 0)
        self._augment_form_controls()
        self._apply_help_texts()

        for label in self.metric_labels.values():
            label.setTextInteractionFlags(Qt.TextSelectableByMouse)

    def _configure_window_layout(self) -> None:
        target_width = 1880
        target_height = 1120
        screen = QApplication.primaryScreen()
        if screen is not None:
            available = screen.availableGeometry()
            target_width = max(1500, min(target_width, available.width() - 40))
            target_height = max(960, min(target_height, available.height() - 40))

        self.setMinimumSize(1500, 960)
        self.resize(target_width, target_height)

        self.leftScrollArea.setMinimumWidth(430)
        self.leftScrollArea.setMaximumWidth(560)

        self.mainSplitter.setChildrenCollapsible(False)
        self.mainSplitter.setHandleWidth(10)
        self.mainSplitter.setStretchFactor(0, 0)
        self.mainSplitter.setStretchFactor(1, 1)

        self.previewGroupBoxRight.setMinimumHeight(640)
        self.previewHostFrame.setMinimumSize(860, 560)
        self.previewHostFrame.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)
        self.previewHostLayout.setContentsMargins(0, 0, 0, 0)

        self.rightPanelLayout.setStretch(0, 0)
        self.rightPanelLayout.setStretch(1, 5)
        self.rightPanelLayout.setStretch(2, 4)

        self._ensure_preview_visible()

    def _augment_form_controls(self) -> None:
        self.customShiftPresetCombo = QComboBox(self)
        self.customShiftPresetCombo.setObjectName("customShiftPresetCombo")
        self.customShiftPresetCombo.setSizeAdjustPolicy(QComboBox.AdjustToContents)
        self.builderFormLayout.insertRow(6, "Atomic cut step", self.customShiftPresetCombo)

        self.projectionCombo = QComboBox(self)
        self.projectionCombo.setObjectName("projectionCombo")
        self.projectionCombo.addItems(PROJECTION_MODES)
        self.projectionCombo.setCurrentText("Orthographic")
        self.previewFormLayout.insertRow(4, "Projection", self.projectionCombo)

    def _apply_help_texts(self) -> None:
        self.symprecLabel.setText("symprec (A)")
        self.symprecLabel.setToolTip(
            "Symmetry matching tolerance in angstrom. Raise it a little if the conventional cell looks wrong."
        )
        self.symprecSpin.setToolTip(self.symprecLabel.toolTip())

        self.maxNormalSearchLabel.setText("Normal search depth")
        self.maxNormalSearchLabel.setToolTip(
            "How far the code searches for a good surface-normal cell. Raise this if slab generation fails or the cell looks awkward."
        )
        self.maxNormalSearchSpin.setToolTip(self.maxNormalSearchLabel.toolTip())

        self.ftolLabel.setText("Plane merge tol (A)")
        self.ftolLabel.setToolTip(
            "How close two nearby atomic planes can be before they are treated as the same cut family."
        )
        self.ftolSpin.setToolTip(self.ftolLabel.toolTip())

        self.tolLabel.setText("Shift match tol")
        self.tolLabel.setToolTip(
            "Numerical tolerance for matching the shifted cut to a valid slab. Raise slightly if custom shifts fail."
        )
        self.tolSpin.setToolTip(self.tolLabel.toolTip())

        self.layerToleranceLabel.setText("Same-layer z tol (A)")
        self.layerToleranceLabel.setToolTip(
            "Atoms closer than this along the surface normal are grouped into the same layer in the layer table."
        )
        self.layerToleranceSpin.setToolTip(self.layerToleranceLabel.toolTip())

        self.surfaceDepthLabel.setText("Surface layers to inspect")
        self.surfaceDepthLabel.setToolTip(
            "How many top and bottom layers are counted as the surface when the app summarizes surface composition."
        )
        self.surfaceDepthSpin.setToolTip(self.surfaceDepthLabel.toolTip())

        self.customShiftLabel.setText("Custom shift (0-1)")
        self.customShiftLabel.setToolTip(
            "Manual cut position along the surface normal. 0 to 1 spans one repeat of the oriented unit cell."
        )
        self.customShiftSpin.setToolTip(self.customShiftLabel.toolTip())
        self.customShiftPresetCombo.setToolTip(
            "Step through cut positions between neighboring atomic layers. One step moves the cut by one atomic layer gap."
        )

        self.cameraLabel.setToolTip("Camera direction for the 3D preview.")
        self.cameraCombo.setToolTip(self.cameraLabel.toolTip())
        self.projectionCombo.setToolTip(
            "Perspective looks more natural. Orthographic removes perspective distortion for a flatter crystallographic view."
        )

        self.loadButton.setToolTip(
            "Manual reload. Demo changes and chosen files also refresh automatically now."
        )
        self.showCellCheckBox.setToolTip("Show the slab/unit-cell box in the preview.")
        self.showBondsCheckBox.setToolTip(
            "Draw approximate bond lines. Large structures may hide them automatically for speed."
        )
        self.fixedLayersLabel.setToolTip(
            "Freeze this many layers from the bottom when exporting Fixed POSCAR."
        )
        self.fixedBottomLayersSpin.setToolTip(self.fixedLayersLabel.toolTip())

    def _set_preview_status(self, message: str | None) -> None:
        text = (message or "").strip()
        self.previewStatusLabel.setText(text)
        self.previewStatusLabel.setVisible(bool(text))

    def _configure_tables(self) -> None:
        for table in (self.terminationTable, self.layerSummaryTable, self.siteTable):
            table.setEditTriggers(QTableWidget.NoEditTriggers)
            table.setSelectionBehavior(QTableWidget.SelectRows)
            table.setSelectionMode(QTableWidget.SingleSelection)
            table.verticalHeader().setVisible(False)
            table.horizontalHeader().setStretchLastSection(True)
        self.logBox.setReadOnly(True)

    def _configure_preview_host(self) -> None:
        if QWebEngineView is not None:
            try:
                self.preview_webview = QWebEngineView(self.previewHostFrame)
                self.preview_webview.setContextMenuPolicy(Qt.NoContextMenu)
                self.preview_webview.setZoomFactor(1.0)
                self.preview_webview.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)
                preview_runtime_dir = (
                    Path(tempfile.gettempdir()) / "slab_studio_qt_webengine" / f"pid_{os.getpid()}"
                )
                preview_runtime_dir.mkdir(parents=True, exist_ok=True)
                profile = self.preview_webview.page().profile()
                profile.setCachePath(str(preview_runtime_dir / "cache"))
                profile.setPersistentStoragePath(str(preview_runtime_dir / "storage"))
                self.preview_webview.loadStarted.connect(
                    lambda: self._set_preview_status("Loading interactive preview...")
                )
                self.preview_webview.loadFinished.connect(self._on_preview_load_finished)
                self._set_preview_host_widget(self.preview_webview)
                self.preview_fallback = QTextBrowser(self.previewHostFrame)
                self.preview_fallback.setOpenExternalLinks(True)
                self.preview_fallback.hide()
                self.previewHostLayout.addWidget(self.preview_fallback)
                self._set_preview_status("")
                return
            except Exception as exc:
                self.preview_webview = None
                self.append_log(f"[WARN] Qt WebEngine preview could not be initialized: {exc}")

        self.preview_fallback = QTextBrowser(self.previewHostFrame)
        self.preview_fallback.setOpenExternalLinks(True)
        self.preview_fallback.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)
        self._set_preview_host_widget(self.preview_fallback)
        self._set_preview_status(
            "Interactive in-window preview is unavailable in this environment. "
            "An embedded summary will appear here instead."
        )

    def _set_preview_host_widget(self, widget: QWidget) -> None:
        while self.previewHostLayout.count():
            item = self.previewHostLayout.takeAt(0)
            if item is None:
                continue
            child_widget = item.widget()
            if child_widget is not None:
                child_widget.setParent(None)
            child_layout = item.layout()
            if child_layout is not None:
                self._clear_layout(child_layout)
        self.previewHostLayout.addWidget(widget)

    def _ensure_preview_visible(self) -> None:
        splitter_width = max(self.mainSplitter.size().width(), self.width(), 1600)
        left_width = min(max(int(splitter_width * 0.30), 460), 540)
        right_width = max(splitter_width - left_width, 980)
        self.mainSplitter.setSizes([left_width, right_width])

    def _clear_layout(self, layout) -> None:
        while layout.count():
            item = layout.takeAt(0)
            if item is None:
                continue
            child_widget = item.widget()
            if child_widget is not None:
                child_widget.setParent(None)
            child_layout = item.layout()
            if child_layout is not None:
                self._clear_layout(child_layout)

    def _connect_signals(self) -> None:
        self.sourceModeCombo.currentIndexChanged.connect(self._sync_source_mode)
        self.sourceModeCombo.currentIndexChanged.connect(lambda *_: self._schedule_auto_load())
        self.demoCombo.currentIndexChanged.connect(self._maybe_auto_load_demo)
        self.browseButton.clicked.connect(self.choose_structure_file)
        self.loadButton.clicked.connect(self.load_structure)
        self.filePathEdit.editingFinished.connect(self._maybe_auto_load_upload)
        self.parserCombo.currentIndexChanged.connect(lambda *_: self._maybe_auto_load_upload())

        self.facetModeCombo.currentIndexChanged.connect(self._sync_facet_mode)
        self.maxIndexSpin.valueChanged.connect(self.update_facet_candidates)
        self.refreshFacetsButton.clicked.connect(self.update_facet_candidates)
        self.facetCombo.currentIndexChanged.connect(self._sync_normal_search_hint)
        self.hSpin.valueChanged.connect(self._sync_normal_search_hint)
        self.kSpin.valueChanged.connect(self._sync_normal_search_hint)
        self.lSpin.valueChanged.connect(self._sync_normal_search_hint)

        self.thicknessModeCombo.currentIndexChanged.connect(self._sync_thickness_mode)
        self.terminationModeCombo.currentIndexChanged.connect(self._sync_termination_mode)
        self.generateButton.clicked.connect(self.generate_slabs)
        self.terminationTable.itemSelectionChanged.connect(self.update_current_preview)

        preview_inputs = [
            self.customShiftSpin,
            self.repASpin,
            self.repBSpin,
            self.orthogonalizeCheckBox,
            self.markerScaleSpin,
            self.cameraCombo,
            self.projectionCombo,
            self.showCellCheckBox,
            self.showBondsCheckBox,
            self.layerToleranceSpin,
            self.surfaceDepthSpin,
            self.fixedBottomLayersSpin,
        ]
        for widget in preview_inputs:
            signal = getattr(widget, "valueChanged", None)
            if signal is None:
                signal = getattr(widget, "currentIndexChanged", None)
            if signal is None:
                signal = getattr(widget, "stateChanged", None)
            if signal is not None:
                signal.connect(self.update_current_preview)

        self.customShiftSpin.valueChanged.connect(self._sync_custom_shift_combo_from_value)
        self.customShiftPresetCombo.currentIndexChanged.connect(self._apply_selected_custom_shift_preset)
        self.openPreviewButton.clicked.connect(self.refresh_preview_in_window)
        self.exportCifButton.clicked.connect(self.export_current_cif)
        self.exportPoscarButton.clicked.connect(self.export_current_poscar)
        self.exportFixedPoscarButton.clicked.connect(self.export_fixed_poscar)

    def append_log(self, message: str) -> None:
        self.logBox.appendPlainText(message)
        self.statusbar.showMessage(message, 5000)

    def show_error(self, title: str, message: str) -> None:
        QMessageBox.critical(self, title, message)
        self.append_log(f"[ERROR] {title}: {message}")

    def choose_structure_file(self) -> None:
        file_path, _ = QFileDialog.getOpenFileName(
            self,
            "Choose structure file",
            str(BASE_DIR),
            "Structure files (*.cif *.vasp *.poscar *.contcar *.xsf *.cssr *.json *.yaml *.yml *.res);;All files (*.*)",
        )
        if file_path:
            self.filePathEdit.setText(file_path)
            self.sourceModeCombo.setCurrentText("Upload")
            self._schedule_auto_load()

    def _schedule_auto_load(self, delay_ms: int = 120) -> None:
        self.auto_load_timer.start(delay_ms)

    def _auto_load_current_source(self) -> None:
        if self.sourceModeCombo.currentText() == "Demo":
            self.load_structure()
            return

        path = self.filePathEdit.text().strip()
        if path and Path(path).exists():
            self.load_structure()

    def _maybe_auto_load_demo(self) -> None:
        if self.sourceModeCombo.currentText() == "Demo":
            self._schedule_auto_load()

    def _maybe_auto_load_upload(self) -> None:
        if self.sourceModeCombo.currentText() != "Upload":
            return
        path = self.filePathEdit.text().strip()
        if path and Path(path).exists():
            self._schedule_auto_load()

    def _sync_source_mode(self) -> None:
        use_demo = self.sourceModeCombo.currentText() == "Demo"
        self.demoCombo.setEnabled(use_demo)
        self.filePathEdit.setEnabled(not use_demo)
        self.browseButton.setEnabled(not use_demo)
        self.parserCombo.setEnabled(not use_demo)

    def _sync_facet_mode(self) -> None:
        use_distinct = self.facetModeCombo.currentText() == "Distinct list"
        self.maxIndexSpin.setEnabled(use_distinct)
        self.facetCombo.setEnabled(use_distinct)
        self.hSpin.setEnabled(not use_distinct)
        self.kSpin.setEnabled(not use_distinct)
        self.lSpin.setEnabled(not use_distinct)

    def _sync_thickness_mode(self) -> None:
        use_planes = self.thicknessModeCombo.currentText() == "hkl planes"
        self.planeCountSpin.setEnabled(use_planes)
        self.slabThicknessSpin.setEnabled(not use_planes)

    def _sync_termination_mode(self) -> None:
        use_custom_shift = self.terminationModeCombo.currentText() == "Custom shift"
        self.customShiftSpin.setEnabled(use_custom_shift)
        self.customShiftPresetCombo.setEnabled(use_custom_shift and self.customShiftPresetCombo.count() > 0)
        if use_custom_shift:
            self._refresh_custom_shift_presets()
        self.update_current_preview()

    def _sync_normal_search_hint(self) -> None:
        try:
            miller = self.current_miller()
        except Exception:
            return
        suggested = max(1, max(abs(value) for value in miller))
        if self.maxNormalSearchSpin.value() < suggested:
            self.maxNormalSearchSpin.setValue(suggested)

    def _refresh_custom_shift_presets(self) -> None:
        self.custom_shift_options = []
        self.customShiftPresetCombo.blockSignals(True)
        self.customShiftPresetCombo.clear()

        if self.structure is None:
            self.customShiftPresetCombo.addItem("Load a structure first", None)
            self.customShiftPresetCombo.setEnabled(False)
            self.customShiftPresetCombo.blockSignals(False)
            return

        try:
            miller = self.current_miller()
            min_slab_size, min_vacuum_size, in_unit_planes, _ = self.current_generation_kwargs()
            slabgen = build_slab_generator(
                structure=self.structure,
                miller=miller,
                min_slab_size=min_slab_size,
                min_vacuum_size=min_vacuum_size,
                in_unit_planes=in_unit_planes,
                lll_reduce=self.lllCheckBox.isChecked(),
                center_the_slab=self.centerCheckBox.isChecked(),
                primitive=self.primitiveCheckBox.isChecked(),
                max_normal_search=self.maxNormalSearchSpin.value(),
            )
            frac_z = np.mod(np.asarray(slabgen.oriented_unit_cell.frac_coords[:, 2], dtype=float), 1.0)
            c_len = float(np.linalg.norm(slabgen.oriented_unit_cell.lattice.matrix[2]))
            frac_tol = max(1e-4, min(0.02, self.tolSpin.value() / max(c_len, 1.0)))

            unique_planes: list[float] = []
            for value in sorted(float(item) for item in frac_z):
                if not unique_planes or abs(value - unique_planes[-1]) > frac_tol:
                    unique_planes.append(value)

            for idx, lower in enumerate(unique_planes):
                upper = unique_planes[(idx + 1) % len(unique_planes)]
                display_upper = upper
                if idx == len(unique_planes) - 1:
                    upper += 1.0
                shift = ((lower + upper) / 2.0) % 1.0
                self.custom_shift_options.append(
                    {
                        "label": (
                            f"Atomic cut {idx + 1}/{len(unique_planes)}: "
                            f"z {lower:.4f} -> {display_upper:.4f}, shift {shift:.4f}"
                        ),
                        "shift": round(float(shift), 4),
                    }
                )
        except Exception:
            self.custom_shift_options = []

        if not self.custom_shift_options:
            self.customShiftPresetCombo.addItem("No atomic cut steps available", None)
            self.customShiftPresetCombo.setEnabled(False)
            self.customShiftPresetCombo.blockSignals(False)
            return

        for option in self.custom_shift_options:
            self.customShiftPresetCombo.addItem(str(option["label"]), option["shift"])

        nearest_index = self._nearest_custom_shift_index(self.customShiftSpin.value())
        self.customShiftPresetCombo.setCurrentIndex(nearest_index)
        self.customShiftPresetCombo.setEnabled(self.terminationModeCombo.currentText() == "Custom shift")
        self.customShiftPresetCombo.blockSignals(False)

        if len(self.custom_shift_options) > 1:
            ordered_shifts = [float(option["shift"]) for option in self.custom_shift_options]
            cyclic_diffs = []
            for idx, shift in enumerate(ordered_shifts):
                next_shift = ordered_shifts[(idx + 1) % len(ordered_shifts)]
                if idx == len(ordered_shifts) - 1:
                    next_shift += 1.0
                cyclic_diffs.append(next_shift - shift)
            min_step = max(min(cyclic_diffs), 0.0005)
            self.customShiftSpin.setSingleStep(round(min_step, 4))

    def _nearest_custom_shift_index(self, value: float) -> int:
        if not self.custom_shift_options:
            return 0
        target = float(value)
        distances = []
        for option in self.custom_shift_options:
            shift = float(option["shift"])
            wrapped_distance = min(abs(target - shift), abs(target - shift + 1.0), abs(target - shift - 1.0))
            distances.append(wrapped_distance)
        return int(min(range(len(distances)), key=distances.__getitem__))

    def _sync_custom_shift_combo_from_value(self) -> None:
        if not self.custom_shift_options or self.customShiftPresetCombo.count() != len(self.custom_shift_options):
            return
        nearest_index = self._nearest_custom_shift_index(self.customShiftSpin.value())
        self.customShiftPresetCombo.blockSignals(True)
        self.customShiftPresetCombo.setCurrentIndex(nearest_index)
        self.customShiftPresetCombo.blockSignals(False)

    def _apply_selected_custom_shift_preset(self) -> None:
        shift = self.customShiftPresetCombo.currentData()
        if shift is None:
            return
        self.customShiftSpin.setValue(float(shift))

    def current_miller(self) -> tuple[int, int, int]:
        if self.facetModeCombo.currentText() == "Distinct list":
            data = self.facetCombo.currentData()
            if data is None:
                raise ValueError("No facet is selected.")
            return tuple(data)
        miller = (self.hSpin.value(), self.kSpin.value(), self.lSpin.value())
        if miller == (0, 0, 0):
            raise ValueError("Manual Miller index (0, 0, 0) is not valid.")
        return miller

    def load_structure(self) -> None:
        try:
            if self.sourceModeCombo.currentText() == "Demo":
                self.structure_raw = build_demo_structure(self.demoCombo.currentText())
            else:
                path = self.filePathEdit.text().strip()
                if not path:
                    raise ValueError("Choose a structure file first.")
                self.structure_raw = load_structure_from_path(path, self.parserCombo.currentText())

            self.structure = normalize_for_surface(
                self.structure_raw,
                use_conventional_cell=self.useConventionalCheckBox.isChecked(),
                symprec=self.symprecSpin.value(),
            )
        except Exception as exc:
            self.show_error("Load failed", str(exc))
            return

        self.update_structure_metrics()
        self.update_facet_candidates()
        self.generate_slabs()
        self.append_log(
            f"Loaded {self.structure.composition.reduced_formula} with {len(self.structure)} sites."
        )

    def update_structure_metrics(self) -> None:
        if self.structure is None:
            return
        self.metric_labels["Formula"].setText(self.structure.composition.reduced_formula)
        self.metric_labels["Sites"].setText(str(len(self.structure)))
        try:
            sg_symbol, sg_number = self.structure.get_space_group_info(symprec=self.symprecSpin.value())
            self.metric_labels["Space group"].setText(f"{sg_symbol} (No. {sg_number})")
        except Exception:
            self.metric_labels["Space group"].setText("N/A")

    def update_facet_candidates(self) -> None:
        if self.structure is None or self.facetModeCombo.currentText() != "Distinct list":
            self._sync_normal_search_hint()
            return

        self.facetCombo.blockSignals(True)
        self.facetCombo.clear()
        try:
            self.facet_candidates = get_facet_candidates(self.structure, self.maxIndexSpin.value())
            for facet in self.facet_candidates:
                label = f"{format_miller(facet)} | d={self.structure.lattice.d_hkl(facet):.3f} A"
                self.facetCombo.addItem(label, facet)
        except Exception as exc:
            self.facetCombo.blockSignals(False)
            self.show_error("Facet enumeration failed", str(exc))
            return
        self.facetCombo.blockSignals(False)
        self._sync_normal_search_hint()

    def current_generation_kwargs(self) -> tuple[float, float, bool, dict]:
        if self.structure is None:
            raise ValueError("No structure is loaded.")
        thickness_mode = self.thicknessModeCombo.currentText()
        slab_size_value = (
            float(self.planeCountSpin.value())
            if thickness_mode == "hkl planes"
            else float(self.slabThicknessSpin.value())
        )
        return slab_generation_kwargs(
            structure=self.structure,
            miller=self.current_miller(),
            thickness_mode=thickness_mode,
            slab_size_value=slab_size_value,
            vacuum_value=float(self.vacuumSpin.value()),
        )

    def generate_slabs(self) -> None:
        if self.structure is None:
            return

        try:
            miller = self.current_miller()
            min_slab_size, min_vacuum_size, in_unit_planes, _ = self.current_generation_kwargs()
            self.generated_slab_dicts = generate_slabs_cached(
                structure_dict=self.structure.as_dict(),
                miller=miller,
                min_slab_size=min_slab_size,
                min_vacuum_size=min_vacuum_size,
                in_unit_planes=in_unit_planes,
                lll_reduce=self.lllCheckBox.isChecked(),
                center_the_slab=self.centerCheckBox.isChecked(),
                primitive=self.primitiveCheckBox.isChecked(),
                max_normal_search=self.maxNormalSearchSpin.value(),
                symmetrize=self.symmetrizeCheckBox.isChecked(),
                ftol=self.ftolSpin.value(),
                tol=self.tolSpin.value(),
                filter_out_sym_slabs=self.filterSymCheckBox.isChecked(),
            )
            self.generated_slabs = [Slab.from_dict(item) for item in self.generated_slab_dicts]
            self.summary_df = (
                summarise_slabs_cached(
                    slab_dicts=self.generated_slab_dicts,
                    layer_tolerance=self.layerToleranceSpin.value(),
                    surface_layers=self.surfaceDepthSpin.value(),
                )
                if self.generated_slab_dicts
                else pd.DataFrame()
            )
        except Exception as exc:
            self.show_error("Slab generation failed", str(exc))
            return

        self._refresh_custom_shift_presets()

        display_df = (
            self.summary_df[
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
            ]
            if not self.summary_df.empty
            else pd.DataFrame()
        )
        self.populate_dataframe_table(self.terminationTable, display_df)
        if self.terminationTable.rowCount() > 0:
            self.terminationTable.selectRow(0)
        else:
            self.update_current_preview()
        self.append_log(
            f"Generated {len(self.generated_slabs)} termination(s) for facet {format_miller(miller)}."
        )

    def selected_generated_index(self) -> int | None:
        rows = self.terminationTable.selectionModel().selectedRows()
        if not rows:
            return 0 if self.generated_slabs else None
        return rows[0].row()

    def current_base_slab(self) -> Slab | None:
        if self.structure is None:
            return None
        if self.terminationModeCombo.currentText() == "Custom shift":
            miller = self.current_miller()
            min_slab_size, min_vacuum_size, in_unit_planes, _ = self.current_generation_kwargs()
            return Slab.from_dict(
                generate_single_slab_cached(
                    structure_dict=self.structure.as_dict(),
                    miller=miller,
                    min_slab_size=min_slab_size,
                    min_vacuum_size=min_vacuum_size,
                    in_unit_planes=in_unit_planes,
                    lll_reduce=self.lllCheckBox.isChecked(),
                    center_the_slab=self.centerCheckBox.isChecked(),
                    primitive=self.primitiveCheckBox.isChecked(),
                    max_normal_search=self.maxNormalSearchSpin.value(),
                    shift=self.customShiftSpin.value(),
                    tol=self.tolSpin.value(),
                )
            )
        selected = self.selected_generated_index()
        if selected is None or selected >= len(self.generated_slabs):
            return None
        return self.generated_slabs[selected]

    def update_current_preview(self) -> None:
        if self.structure is None:
            return
        try:
            self.active_base_slab = self.current_base_slab()
        except Exception as exc:
            self.show_error("Preview update failed", str(exc))
            return

        if self.active_base_slab is None:
            self.active_preview_slab = None
            self.active_export_slab = None
            self.active_layer_site_df = pd.DataFrame()
            self.active_layer_summary_df = pd.DataFrame()
            self.active_meta = None
            self.metric_labels["Active slab"].setText("-")
            self.populate_dataframe_table(self.layerSummaryTable, pd.DataFrame())
            self.populate_dataframe_table(self.siteTable, pd.DataFrame())
            self.update_embedded_preview()
            return

        self.active_preview_slab = make_builder_slab(
            slab=self.active_base_slab,
            rep_a=self.repASpin.value(),
            rep_b=self.repBSpin.value(),
            orthogonalize_c=self.orthogonalizeCheckBox.isChecked(),
        )
        self.active_meta = summarize_single_slab(
            slab=self.active_preview_slab,
            termination_label="active",
            layer_tolerance=self.layerToleranceSpin.value(),
            surface_layers=self.surfaceDepthSpin.value(),
        )
        self.active_layer_site_df, self.active_layer_summary_df = layer_tables(
            self.active_preview_slab,
            tolerance_angstrom=self.layerToleranceSpin.value(),
        )
        self.fixedBottomLayersSpin.setMaximum(max(0, len(self.active_layer_summary_df)))
        self.active_export_slab, self.active_fixed_atoms = apply_fixed_bottom_layers(
            structure=self.active_preview_slab,
            layer_site_df=self.active_layer_site_df,
            n_fixed_layers=self.fixedBottomLayersSpin.value(),
        )

        layer_df = (
            self.active_layer_summary_df[["layer_id", "n_sites", "composition", "z_center", "z_min", "z_max"]]
            if not self.active_layer_summary_df.empty
            else pd.DataFrame()
        )
        site_df = (
            self.active_layer_site_df[
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
            ]
            if not self.active_layer_site_df.empty
            else pd.DataFrame()
        )
        self.populate_dataframe_table(self.layerSummaryTable, layer_df)
        self.populate_dataframe_table(self.siteTable, site_df)
        self.update_preview_metrics()
        self._sync_custom_shift_combo_from_value()
        self.update_embedded_preview()

    def update_preview_metrics(self) -> None:
        if self.structure is None:
            return
        self.metric_labels["Facet"].setText(format_miller(self.current_miller()))
        if self.active_meta is None:
            return
        active_name = (
            f"Custom shift {self.customShiftSpin.value():.4f}"
            if self.terminationModeCombo.currentText() == "Custom shift"
            else f"Generated #{self.selected_generated_index() or 0}"
        )
        self.metric_labels["Active slab"].setText(active_name)
        self.metric_labels["Shift"].setText(str(self.active_meta["shift"]))
        self.metric_labels["Atoms"].setText(str(self.active_meta["atoms"]))
        self.metric_labels["Layers~"].setText(str(self.active_meta["atomic_layers~"]))
        self.metric_labels["Top surface"].setText(str(self.active_meta["top_surface"]))
        self.metric_labels["Bottom surface"].setText(str(self.active_meta["bottom_surface"]))
        self.metric_labels["Vacuum (A)"].setText(str(self.active_meta["vacuum(A)"]))
        self.metric_labels["Slab thickness (A)"].setText(str(self.active_meta["slab_thickness(A)"]))

    def populate_dataframe_table(self, table: QTableWidget, df: pd.DataFrame) -> None:
        table.clear()
        if df.empty:
            table.setRowCount(0)
            table.setColumnCount(0)
            return
        safe_df = df.copy().astype(object)
        for column in safe_df.columns:
            safe_df[column] = safe_df[column].apply(
                lambda value: "Yes" if value is True else "No" if value is False else str(value)
            )
        table.setColumnCount(len(safe_df.columns))
        table.setRowCount(len(safe_df))
        table.setHorizontalHeaderLabels([str(column) for column in safe_df.columns])
        for row_idx, row in safe_df.iterrows():
            for col_idx, value in enumerate(row.tolist()):
                item = QTableWidgetItem(value)
                item.setFlags(item.flags() & ~Qt.ItemIsEditable)
                table.setItem(row_idx, col_idx, item)
        table.resizeRowsToContents()

    def current_export_root(self) -> str:
        miller = self.current_miller()
        if self.terminationModeCombo.currentText() == "Custom shift":
            tag = f"shift{self.customShiftSpin.value():.4f}".replace(".", "p")
        else:
            tag = f"term{self.selected_generated_index() or 0}"
        return (
            f"slab_{file_tag_from_miller(miller)}_{tag}_"
            f"a{self.repASpin.value()}_b{self.repBSpin.value()}"
            f"{'_ortho' if self.orthogonalizeCheckBox.isChecked() else ''}"
        )

    def _safe_text(self, value: object) -> str:
        return html.escape(str(value))

    def build_preview_html(self) -> str:
        if self.active_preview_slab is None:
            return "<html><body><p>No active slab preview.</p></body></html>"
        oriented_bulk = getattr(self.active_base_slab, "oriented_unit_cell", self.structure)
        bulk_fig = structure_figure(
            structure=oriented_bulk,
            show_cell=self.showCellCheckBox.isChecked(),
            show_bonds=self.showBondsCheckBox.isChecked(),
            marker_scale=self.markerScaleSpin.value(),
            camera_preset=self.cameraCombo.currentText(),
            projection_mode=self.projectionCombo.currentText(),
        )
        slab_fig = structure_figure(
            structure=self.active_preview_slab,
            show_cell=self.showCellCheckBox.isChecked(),
            show_bonds=self.showBondsCheckBox.isChecked(),
            marker_scale=self.markerScaleSpin.value(),
            camera_preset=self.cameraCombo.currentText(),
            projection_mode=self.projectionCombo.currentText(),
        )
        warnings = preview_warning_messages(
            self.active_preview_slab,
            self.showBondsCheckBox.isChecked(),
        )
        warning_html = ""
        if warnings:
            warning_items = "".join(f"<li>{message}</li>" for message in warnings)
            warning_html = f"<ul>{warning_items}</ul>"
        plotly_bundle = sanitize_plotly_javascript(get_plotlyjs())
        bulk_html = bulk_fig.to_html(
            full_html=False,
            include_plotlyjs=False,
            default_width="100%",
            default_height="470px",
        )
        slab_html = slab_fig.to_html(
            full_html=False,
            include_plotlyjs=False,
            default_width="100%",
            default_height="470px",
        )
        return f"""
        <html>
        <head>
          <meta charset="utf-8">
          <meta name="viewport" content="width=device-width, initial-scale=1">
          <title>Slab Studio Qt Preview</title>
          <script type="text/javascript">
          {plotly_bundle}
          </script>
          <style>
            html, body {{ height: 100%; margin: 0; }}
            body {{
              font-family: Segoe UI, Arial, sans-serif;
              padding: 16px;
              box-sizing: border-box;
              background: #f5f7fa;
              color: #1f2933;
            }}
            .grid {{
              display: grid;
              grid-template-columns: repeat(2, minmax(420px, 1fr));
              gap: 16px;
              align-items: stretch;
            }}
            .card {{
              border: 1px solid #d8dee9;
              border-radius: 12px;
              padding: 12px;
              background: white;
              min-height: 540px;
              box-shadow: 0 10px 30px rgba(15, 23, 42, 0.08);
            }}
            h2 {{ margin: 0 0 12px; font-size: 20px; }}
            table {{ border-collapse: collapse; }}
            td {{ padding: 4px 10px 4px 0; }}
            .js-plotly-plot, .plot-container, .plotly-graph-div {{
              width: 100% !important;
              min-height: 470px !important;
            }}
            @media (max-width: 1200px) {{
              .grid {{ grid-template-columns: 1fr; }}
            }}
          </style>
        </head>
        <body>
          <table>
            <tr><td><b>Facet</b></td><td>{self._safe_text(format_miller(self.current_miller()))}</td></tr>
            <tr><td><b>Repeat</b></td><td>{self.repASpin.value()} x {self.repBSpin.value()} x 1</td></tr>
          </table>
          {warning_html}
          <div class="grid">
            <div class="card"><h2>Oriented bulk</h2>{bulk_html}</div>
            <div class="card"><h2>Active slab</h2>{slab_html}</div>
          </div>
        </body>
        </html>
        """

    def build_preview_artifact(self) -> Path:
        preview_dir = Path(tempfile.gettempdir()) / "slab_studio_qt_preview"
        preview_dir.mkdir(parents=True, exist_ok=True)
        html_path = preview_dir / f"{self.current_export_root()}_preview.html"
        html_path.write_text(self.build_preview_html(), encoding="utf-8")
        self.last_preview_html = html_path
        return html_path

    def preview_summary_html(self) -> str:
        if self.active_preview_slab is None or self.active_meta is None:
            return "<html><body><p>No active slab preview.</p></body></html>"

        warnings = preview_warning_messages(
            self.active_preview_slab,
            self.showBondsCheckBox.isChecked(),
        )
        warning_html = ""
        if warnings:
            warning_items = "".join(f"<li>{self._safe_text(item)}</li>" for item in warnings)
            warning_html = f"<ul>{warning_items}</ul>"
        reason_text = (
            "Interactive preview is currently unavailable in this environment."
            if self.preview_webview is None
            else "Interactive preview is temporarily unavailable inside the embedded view."
        )
        return f"""
        <html>
        <head>
          <meta charset="utf-8">
          <style>
            body {{ font-family: Segoe UI, Arial, sans-serif; padding: 14px; }}
            .pill {{ display: inline-block; padding: 2px 8px; border-radius: 999px; background: #eee; margin-right: 6px; }}
          </style>
        </head>
        <body>
          <h3>Preview summary</h3>
          <div>
            <span class="pill">Facet {self._safe_text(format_miller(self.current_miller()))}</span>
            <span class="pill">Atoms {self.active_meta['atoms']}</span>
            <span class="pill">Layers~ {self.active_meta['atomic_layers~']}</span>
          </div>
          <p>{self._safe_text(reason_text)}</p>
          {warning_html}
          <p>Use <b>Refresh Preview</b> to regenerate the embedded Plotly view inside this window.</p>
        </body>
        </html>
        """

    def _on_preview_load_finished(self, ok: bool) -> None:
        if ok:
            if self.preview_fallback is not None:
                self.preview_fallback.hide()
            if self.preview_webview is not None:
                self.preview_webview.show()
            self._set_preview_status("")
        else:
            if self.preview_webview is not None:
                self.preview_webview.hide()
            if self.preview_fallback is not None:
                self.preview_fallback.setHtml(self.preview_summary_html())
                self.preview_fallback.show()
            self._set_preview_status(
                "Preview load failed inside the embedded view. An embedded summary is shown here instead."
            )

    def _preview_url(self, artifact: Path) -> QUrl:
        preview_url = QUrl.fromLocalFile(str(artifact))
        preview_url.setQuery(f"v={time.time_ns()}")
        return preview_url

    def update_embedded_preview(self) -> None:
        if self.active_preview_slab is None:
            if self.preview_webview is not None:
                self.preview_webview.setHtml("<html><body><p>No active slab preview.</p></body></html>")
            if self.preview_fallback is not None:
                self.preview_fallback.setHtml("<p>No active slab preview.</p>")
                self.preview_fallback.show()
            self._set_preview_status("No active slab preview.")
            return

        self._ensure_preview_visible()

        if self.preview_webview is not None:
            try:
                artifact = self.build_preview_artifact()
                if self.preview_fallback is not None:
                    self.preview_fallback.hide()
                self.preview_webview.show()
                self.preview_webview.load(self._preview_url(artifact))
            except Exception as exc:
                self._set_preview_status(
                    "Interactive preview could not be updated. An embedded summary is shown here instead."
                )
                self.append_log(f"[WARN] Embedded preview update failed: {exc}")
                if self.preview_fallback is not None:
                    self.preview_fallback.setHtml(self.preview_summary_html())
                    self.preview_fallback.show()
                if self.preview_webview is not None:
                    self.preview_webview.hide()
        elif self.preview_fallback is not None:
            self.preview_fallback.setHtml(self.preview_summary_html())
            self.preview_fallback.show()
            self._set_preview_status(
                "Preview fallback is showing a summary because embedded Qt WebEngine preview is unavailable."
            )

    def refresh_preview_in_window(self) -> None:
        self._ensure_preview_visible()
        self._set_preview_status("Refreshing interactive preview...")
        self.update_current_preview()
        if self.active_preview_slab is None:
            self.show_error("Preview unavailable", "Generate or select a slab first.")
            return
        if self.preview_webview is not None:
            self.preview_webview.setFocus(Qt.OtherFocusReason)
        elif self.preview_fallback is not None:
            self.preview_fallback.setFocus(Qt.OtherFocusReason)
        self.append_log("Refreshed the embedded preview.")

    def save_text(self, suggested_name: str, dialog_filter: str, text: str) -> None:
        file_path, _ = QFileDialog.getSaveFileName(self, "Save file", suggested_name, dialog_filter)
        if not file_path:
            return
        Path(file_path).write_text(text, encoding="utf-8")
        self.append_log(f"Saved: {file_path}")

    def export_current_cif(self) -> None:
        if self.active_preview_slab is None:
            self.show_error("Export unavailable", "Generate or select a slab first.")
            return
        self.save_text(
            suggested_name=f"{self.current_export_root()}.cif",
            dialog_filter="CIF files (*.cif);;All files (*.*)",
            text=self.active_preview_slab.to(fmt="cif"),
        )

    def export_current_poscar(self) -> None:
        if self.active_preview_slab is None:
            self.show_error("Export unavailable", "Generate or select a slab first.")
            return
        self.save_text(
            suggested_name=f"POSCAR_{self.current_export_root()}",
            dialog_filter="POSCAR files (*)",
            text=str(Poscar(self.active_preview_slab)),
        )

    def export_fixed_poscar(self) -> None:
        if self.active_export_slab is None:
            self.show_error("Export unavailable", "Generate or select a slab first.")
            return
        self.save_text(
            suggested_name=f"POSCAR_{self.current_export_root()}_fix{self.fixedBottomLayersSpin.value()}",
            dialog_filter="POSCAR files (*)",
            text=str(Poscar(self.active_export_slab)),
        )


def main() -> int:
    app = QApplication(sys.argv)
    window = SlabStudioQtWindow()
    window.show()
    return app.exec_()


if __name__ == "__main__":
    raise SystemExit(main())
