import os
import math
import sys
from os import environ
from io import StringIO
from PyQt5 import uic
from PyQt5.QtWidgets import *
from PyQt5.QtGui import QDragEnterEvent, QDropEvent, QTextCursor
from PyQt5.QtCore import QTimer
from ase.io import read, write

Desktop_directory = os.path.expanduser('~') + "/Desktop"
Recent_used_directory = Desktop_directory
new_file_name = ''

ui_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'main.ui')
form_class = uic.loadUiType(ui_path)[0]

def suppress_qt_warnings():
    environ["QT_DEVICE_PIXEL_RATIO"] = "0"
    environ["QT_AUTO_SCREEN_SCALE_FACTOR"] = "1"
    environ["QT_SCREEN_SCALE_FACTORS"] = "1"
    environ["QT_SCALE_FACTOR"] = "1"

class IncarGeneratorApp(QMainWindow, form_class):
    def __init__(self):
        super().__init__()

        self.setupUi(self)
        #self.setAcceptDrops(True)
        self.view_button.clicked.connect(self.structure_view)

        self.save_poscar_button.clicked.connect(self.save_poscar)

        self.incar_option_apply_button.clicked.connect(self.apply_incar_value)

        self.z_value_apply.clicked.connect(self.fix_atoms)
        self.z_value.returnPressed.connect(self.fix_atoms)
        
        self.POSCAR.dragEnterEvent = self.poscar_drag_enter_event
        self.POSCAR.dropEvent = self.poscar_drop_event

        self.visualization_tab.dragEnterEvent = self.visualization_tab_drag_enter_event
        self.visualization_tab.dropEvent = self.visualization_tab_drop_event

        self.file_convert_label.dragEnterEvent = self.convert_drag_enter_event
        self.file_convert_label.dropEvent = self.convert_drop_event
        

    def apply_incar_value(self):
        self.textbox_incar.clear()
        incar = {
            "# General" : 1,
            "PREC" : "Accurate",
            "SYMPREC" : "1E-8",
            "ISTART" : "",
            "ICHARG" : "",
            "ISPIN" : "2",
            "GGA" : "",
            "IVDW" : "",

            "# Write Flags" : None,
            "LWAVE" : "",
            "LCHARG" : "",
            "LAECHG" : "",
            "LVHAR" : "",
            "LORBIT" : "11",

            "# Dipole Correction" : None,
            "LDIPOL" : "",
            "IDIPOL" : "",

            "# Electronic Relaxation" : None,
            "ALGO" : "",
            "ENCUT" : "400",
            "EDIFF" : "1E-7",
            "NELM" : "300",
            "LREAL" : "Auto", 
            "MAXMIX" : "",

            "# Ionic Relaxation" : None,
            "IBRION" : "",
            "NSW" : "",
            "POTIM" : "",
            "ISIF" : "2",
            "EDIFFG" : "",
            "ISYM" : "",
            "NFREE" : "",

            "# DOS" : None,
            "ISMEAR" : "0",
            "SIGMA" : "0.05",
            "EMAX"  : "",
            "EMIN"  : "",
            "NEDOS" : "",

            "# Solvation" : None,
            "LSOL" : "",
            "TAU" : "",

            "# HSE" : None,
            "LHFCALC" : "",
            "HFSCREEN" : "",
            "TIME" : "",

            "# Parallelization": None,
            "LPLANE" : ".TRUE.",
            "NCORE" : "1"
        }

        if self.comboBox_2.currentText() in ["Solvation", "HSE"]:
            incar["ISTART"] = "1"
        else:
            incar["ISTART"] = "0"

        if self.comboBox_2.currentText() == "HSE":
            incar["ICHARG"] = "11"
        else:
            incar["ICHARG"] = "2"
        
        if self.comboBox_3.currentText() == "PBE":
            incar["GGA"] = "PE"
        elif self.comboBox_3.currentText() == "RPBE":
            incar["GGA"] = "RP"
        elif self.comboBox_3.currentText() == "PW91":
            incar["GGA"] = "91"
        else:
            pass

        if self.comboBox_4.currentText() == "D2":
            incar["IVDW"] = "1"
        elif self.comboBox_4.currentText() == "D3":
            incar["IVDW"] = "11"
        else:
            pass
        
        if self.comboBox_2.currentText() in ["Relaxation", "Solvation", "Vibration"]:
            incar["LWAVE"] = ".FALSE."
            incar["LCHARG"] = ".FALSE."
        elif self.comboBox_2.currentText() in ["Single Point", "HSE"]:
            incar["LWAVE"] = ".TRUE."
            incar["LCHARG"] = ".TRUE."
            incar["LAECHG"] = ".TRUE."
            incar["LVHAR"] = ".TRUE."
        else:
            pass

        if self.comboBox_1.currentText() == "Surface":
            incar["LDIPOL"] = ".TRUE."
            incar["IDIPOL"] = "3"
        elif self.comboBox_1.currentText() == "Bulk":
            incar["ISIF"] = "3"
        else :
            pass
        
        if self.comboBox_1.currentText() == "Molecule":
            incar["ALGO"] = "Normal"
        elif self.comboBox_2.currentText() == "HSE":
            incar["ALGO"] = "Damped"
        else:
            incar["ALGO"] = "Fast"
        
        cut_off_energy_value = self.cut_off_energy.value()
        incar["ENCUT"] = cut_off_energy_value

        if self.comboBox_2.currentText() == "Vibration":
            incar["MAXMIX"] = "60"
        else:
            pass
        
        if self.comboBox_2.currentText() == "Relaxation":
            incar["IBRION"] = "2"
            incar["NSW"] = "2000"
            incar["POTIM"] = "0.5"
            incar["EDIFFG"] = "1E-6"
            incar["ISYM"] = "0"
        elif self.comboBox_2.currentText() == "Vibration":
            incar["IBRION"] = "5"
            incar["NSW"] = "2000"
            incar["POTIM"] = "0.015"
            incar["EDIFFG"] = "1E-6"
            incar["ISYM"] = "0"
            incar["NFREE"] = "2"
        else:
            incar["IBRION"] = "-1"
            incar["NSW"] = "0"

        if self.comboBox_2.currentText() == "Solvation":
            incar["LSOL"] = ".TRUE."
            incar["TAU"] = "0"
        else:
            pass
        
        if self.comboBox_2.currentText() == "Single Point":
            incar["EMAX"] = "20"
            incar["EMIN"] = "-20"
            incar["NEDOS"] = "4000"

        if self.comboBox_2.currentText() == "HSE":
            incar["LHFCALC"] = ".TRUE."
            incar["HFSCREEN"] = "0.2"
            incar["TIME"] = "0.4"
        else:
            pass
        
        incar_text = ""
        for key in incar:
            if key.startswith("#"):
                if incar[key] == 1:
                    incar_text += f"{key}\n"
                else:
                    incar_text += f"\n{key}\n"
            elif incar[key] == "":
                pass
            else:
                incar_text += f"{key.ljust(10)} = {incar[key]} \n"
        
        self.textbox_incar.setPlainText(incar_text)


    def poscar_drag_enter_event(self, event):
        if event.mimeData().hasUrls() :
            event.accept()
        else:
            event.ignore()
    
    def poscar_drop_event(self, event):
        if event.mimeData().hasUrls() :
            self.drop_for_read_poscar(event)
        else:
            event.ignore()

    def visualization_tab_drag_enter_event(self, event):
        if event.mimeData().hasUrls() :
            event.accept()
        else:
            event.ignore()

    def visualization_tab_drop_event(self, event):
        if event.mimeData().hasUrls() :
            self.visualize_strucutre_file(event)
        else:
            event.ignore()

    def visualize_strucutre_file(self, event):
        for url in event.mimeData().urls():
            file_path = url.toLocalFile()
            clean_path = file_path.strip('{}')
            atoms = read(clean_path)
            #output = StringIO()
            #write(output, atoms)
            #structure_file_content = atoms
            #self.textbox_structure_file.clear()
            #self.textbox_structure_file.insertPlainText(atoms)

    def drop_for_read_poscar(self, event):
        for url in event.mimeData().urls():
            file_path = url.toLocalFile()
            clean_path = file_path.strip('{}')
            atoms = read(clean_path)
            output = StringIO()
            write(output, atoms, format='vasp')
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

    def save_poscar(self):
        global Recent_used_directory
        file_path, _ = QFileDialog.getSaveFileName(self, "Save POSCAR File", Recent_used_directory, "VASP files (*.vasp);;CIF files (*.cif);;All files (*.*)")
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
        write(output, visualize, format='vasp')
        vasp_content = output.getvalue()
        self.textbox_poscar.clear()
        self.textbox_poscar.insertPlainText(vasp_content)
        

    def write_kpoints(self):
        # Not Yet
        pass

    def convert_drag_enter_event(self, event):
        if event.mimeData().hasUrls() :
            event.accept()
        else:
            event.ignore()
    
    def convert_drop_event(self, event):
        if event.mimeData().hasUrls() :
            self.drop_for_file_convert(event)
        else:
            event.ignore()

    def drop_for_file_convert(self, event):
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
    suppress_qt_warnings()
    app = QApplication(sys.argv)
    mainWindow = IncarGeneratorApp()
    mainWindow.show()
    sys.exit(app.exec_())