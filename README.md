# VASP Input Generator

VASP Input Generator is a PyQt5 desktop application for preparing common VASP input files and handling simple structure editing tasks with ASE.

## Overview

This repository contains a GUI workflow for:

- generating `INCAR` templates from common calculation presets
- editing and saving `POSCAR` structures
- creating `KPOINTS` from a user-defined k-spacing
- converting between `CIF` and `VASP` structure files
- opening structures in the ASE GUI for quick inspection

The project also includes a lightweight structure viewer script and several Jupyter notebooks for post-processing and analysis.

## Features

### INCAR generation

The main application can generate an `INCAR` template based on:

- model type: `Bulk`, `Surface`, `Molecule`
- calculation type: `Relaxation`, `Single Point`, `Vibration`, `Solvation`, `NEB`, `HSE`
- exchange-correlation functional: `PBE`, `RPBE`, `PW91`
- van der Waals correction: `None`, `D2`, `D3`
- optional flags: `DFT+U`, `WAVECAR`, `CHGCAR`, `DOS`, `GC`

### POSCAR utilities

- drag and drop structure files into the GUI
- load `.vasp` and `.cif` structures into the POSCAR editor
- open the current structure in the ASE GUI
- sort atoms in the structure
- wrap coordinates back into the unit cell
- apply selective dynamics flags based on a user-defined `z` threshold
- save the current structure to `.vasp`, `.cif`, `.pdb`, or `.xyz`

### KPOINTS generation

- create a Gamma-centered `KPOINTS` mesh from lattice vector lengths and a user-defined k-spacing

### File conversion

- convert `.cif` to `.vasp`
- convert `.vasp` to `.cif`

## Repository Structure

- `VIG_v2026.1.py`: main PyQt5 application
- `main.ui`: Qt Designer UI file used by the main application
- `Visualization.py`: simple ASE-based structure viewer script
- `VASP_Analysis_Tools.ipynb`: analysis notebook
- `DOS_plot_smearing.ipynb`: DOS plotting notebook
- `COHP_Analysis.ipynb`: COHP analysis notebook

## Requirements

- Python 3
- `PyQt5`
- `ase`
- `tkinter` for `Visualization.py` file selection dialog

## Installation

Install the required Python packages:

```bash
pip install pyqt5 ase
```

## Usage

Run the main GUI:

```bash
python VIG_v2026.1.py
```

Run the standalone structure viewer:

```bash
python Visualization.py
```

## Typical Workflow

1. Open the main application.
2. Paste or drag a structure file into the `POSCAR` tab.
3. Adjust the model type, calculation type, functional, and optional flags in the `INCAR` tab.
4. Click `Apply Options` to generate the `INCAR` text.
5. Generate `KPOINTS` from the `KPOINTS` tab if needed.
6. Use the `POSCAR` and `Tools` tabs for structure editing or file conversion.
7. Save the edited structure in the desired format.

## Notes

- The generated `INCAR` content is a convenience template and should be reviewed before production VASP calculations.
- VASP itself is not included in this repository.
- The GUI depends on `main.ui`, so keep it in the same directory as `VIG_v2026.1.py`.
