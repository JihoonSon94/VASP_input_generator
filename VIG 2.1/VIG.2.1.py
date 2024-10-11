import os
import math
import sys
from io import StringIO
from PyQt5 import uic
from PyQt5.QtWidgets import *
from PyQt5.QtGui import QDragEnterEvent, QDropEvent
from ase.io import read, write
from ase.visualize import view

Desktop_directory = os.path.expanduser('~') + "/Desktop"
Recent_used_directory = Desktop_directory
new_file_name = ''

ui_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'main.ui')
form_class = uic.loadUiType(ui_path)[0]

class IncarGeneratorApp(QMainWindow, form_class):
    def __init__(self):
        super().__init__()
        self.setupUi(self)
        self.setAcceptDrops(True)
        self.view_button.clicked.connect(self.structure_view)
        self.z_value_apply.clicked.connect(self.fix_atoms)
        self.textbox_poscar.dragEnterEvent = self.poscar_drag_enter_event
        self.textbox_poscar.dropEvent = self.poscar_drop_event
        self.z_value.returnPressed.connect(self.fix_atoms)

    def apply_incar_value(self):
        self.text1.clear()
        # General
        general = ["#General","PREC    = Accurate","SYMPREC = 1E-8","ISTART  = 0","ICHARG  = 2","ISPIN   = 2","\n"]
        if combobox_model_type == 

    def poscar_drag_enter_event(self, event):
        if event.mimeData().hasUrls() :
            event.accept()
        else:
            event.ignore()

    def poscar_drop_event(self, event):
        if event.mimeData().hasUrls() :
            self.drop_for_tab2(event)
        else:
            event.ignore()

    def drop_for_tab2(self, event):
        for url in event.mimeData().urls():
            file_path = url.toLocalFile()
            clean_path = file_path.strip('{}')
            atoms = read(clean_path)
            output = StringIO()
            write(output, atoms, format='vasp', sort=True)
            vasp_content = output.getvalue()
            self.textbox_poscar.clear()
            self.textbox_poscar.insertPlainText(vasp_content)

    def read_poscar(self):
        global Recent_used_directory
        filename, _ = QFileDialog.getOpenFileName(self, "Open POSCAR File", Recent_used_directory, "VASP files (*.vasp);;All files (*.*)")
        if filename:
            with open(filename, 'r') as file:
                file_content = file.read()
            self.textbox_poscar.setText(file_content)
            Recent_used_directory = os.path.dirname(filename)

    def fix_atoms(self):
        lines = self.textbox_poscar.toPlainText().splitlines()
        print(lines)
        number_of_each_atoms = lines[6].split()
        total_num_of_atom = sum([int(num) for num in number_of_each_atoms])

        if len(lines) > 7 and lines[7].lower().startswith("s" or "S"):
            # Replace line 8 with "Selective dynamics"
            lines[7] = "Selective dynamics"
        else:
            # Insert "Selective dynamics" at line 8
            lines.insert(7, "Selective dynamics")

        input_value = float(self.z_value.text())
        position_range = total_num_of_atom + 9

        # Modify atom positions based on z_value
        for i in range(9, position_range):
            atom_position_list = lines[i].split()
            atom_position_string = ""

            for j in range(3):
                if float(atom_position_list[j]) < 0 or float(atom_position_list[j]) >= 10:
                    atom_position_string += " " + "{:.16f}".format(float(atom_position_list[j]))
                else:
                    atom_position_string += "  " + "{:.16f}".format(float(atom_position_list[j]))

            if float(atom_position_list[2]) >= input_value:
                new_line = atom_position_string + "   T   T   T"
            else:
                new_line = atom_position_string + "   F   F   F"

            lines[i] = new_line

        updated_text = "\n".join(lines)
        self.textbox_poscar.clear()
        self.textbox_poscar.setPlainText(updated_text)

    def write_poscar(self):
        global Recent_used_directory
        file_path, _ = QFileDialog.getSaveFileName(self, "Save POSCAR File", Recent_used_directory, "VASP files (*.vasp);;All files (*.*)")
        if file_path:
            content = self.textbox_poscar.toPlainText()
            string_io = StringIO(content)
            atoms = read(string_io, format='vasp')
            write(file_path, atoms)

    def structure_view(self):
        structure_content = self.textbox_poscar.toPlainText()
        structure_io = StringIO(structure_content)
        visualize = read(structure_io, format='vasp')
        visualize.edit()
        output = StringIO()
        write(output, visualize, format='vasp', sort=True)
        vasp_content = output.getvalue()
        self.textbox_poscar.clear()
        self.textbox_poscar.insertPlainText(vasp_content)

    def write_kpoints(self):
        # Implementation for writing k-points
        pass

    def make_shell(self):
        # Implementation for generating shell script
        pass

    def dragEnterEvent(self, event: QDragEnterEvent):
        if event.mimeData().hasUrls():
            event.acceptProposedAction()

    def drop_for_file_convert(self, event: QDropEvent):
        for url in event.mimeData().urls():
            file_path = url.toLocalFile()
            if file_path.lower().endswith('.cif'):
                self.convert_cif2vasp(file_path)
            elif file_path.lower().endswith('.vasp'):
                self.convert_vasp2cif(file_path)

    def convert_cif2vasp(self, file_path):
        if file_path:
            atoms = read(file_path, format='cif')
            file_name = os.path.basename(file_path)
            file_name_without_ext = os.path.splitext(file_name)[0]
            save_path = os.path.join(os.path.dirname(file_path), file_name_without_ext + '.vasp')
            write(save_path, atoms, format='vasp', direct=False, sort=True)
            print(f"File saved as {save_path}")

    def convert_vasp2cif(self, file_path):
        if file_path:
            atoms = read(file_path, format='vasp')
            file_name = os.path.basename(file_path)
            file_name_without_ext = os.path.splitext(file_name)[0]
            save_path = os.path.join(os.path.dirname(file_path), file_name_without_ext + '.cif')
            write(save_path, atoms, format='cif')
            print(f"File saved as {save_path}")

if __name__ == '__main__':
    app = QApplication(sys.argv)
    mainWindow = IncarGeneratorApp()
    mainWindow.show()
    sys.exit(app.exec_())