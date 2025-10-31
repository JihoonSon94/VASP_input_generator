import os
from ase.io import read
from ase.gui.gui import GUI
from tkinter import Tk
from tkinter.filedialog import askopenfilename

def open_file_dialog():
    """Select structure file through filedialog box"""
    root = Tk()
    root.withdraw()
    fname = askopenfilename(
        title="Select structure file",
        filetypes=[
            ("All supported files", ("*.traj", "*.cif", "*.xyz",
                                     "*.vasp", "*.poscar", "*.contcar",
                                     "*POSCAR*", "*CONTCAR*", "*.pdb")),
            ("Trajectory files", ("*.traj",)),
            ("VASP files", ("*.vasp", "*.poscar", "*.contcar", "*POSCAR*", "*CONTCAR*")),
            ("CIF files", ("*.cif",)),
            ("XYZ files", ("*.xyz",)),
            ("PDB files", ("*.pdb",)),
            ("All files", ("*.*",))
        ]
    )
    root.destroy()
    return fname

def load_structure(filename):
    """Load structure file and return list[Atoms]"""
    if not filename:
        print("❌ No file selected.")
        return None

    if not os.path.exists(filename):
        print(f"❌ Error: File '{filename}' does not exist.")
        return None

    try:
        print(f"📁 Reading file: {os.path.basename(filename)}")
        try:
            images = read(filename, index=":")
        except Exception:
            images = None

        if images is None or (isinstance(images, list) and len(images) == 0):
            single = read(filename)
            images = [single]
        elif not isinstance(images, list):
            images = [images]

        return images

    except Exception as e:
        print(f"❌ Failed to read file '{os.path.basename(filename)}': {e}")
        return None

def open_file():
    """Main entry point: open file and show ASE GUI"""
    filename = open_file_dialog()
    images = load_structure(filename)
    if images is None:
        return

    print("🖥️ Opening ASE GUI...")
    gui = GUI(images=images)
    gui.run()

if __name__ == "__main__":
    open_file()