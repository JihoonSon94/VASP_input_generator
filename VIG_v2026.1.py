import os
import sys
import math
from os import environ
from io import StringIO
from PyQt5 import uic
from PyQt5.QtWidgets import *
from PyQt5.QtCore import Qt
from ase import Atoms
from ase.io import read, write
from ase.gui.gui import GUI

Desktop_directory = os.path.expanduser('~') + "/Desktop"
Recent_used_directory = Desktop_directory
new_file_name = ''

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
main_ui_path = os.path.join(BASE_DIR, 'main.ui')
form_class = uic.loadUiType(main_ui_path)[0]

def suppress_qt_warnings():
    environ["QT_DEVICE_PIXEL_RATIO"] = "0"
    environ["QT_AUTO_SCREEN_SCALE_FACTOR"] = "1"
    environ["QT_SCREEN_SCALE_FACTORS"] = "1"
    environ["QT_SCALE_FACTOR"] = "1"

class IncarGeneratorApp(QDialog, form_class):
    def __init__(self):
        super().__init__()
        self.setWindowFlags(Qt.WindowCloseButtonHint | Qt.WindowMaximizeButtonHint | Qt.WindowMinimizeButtonHint)
        self.setupUi(self)
        
        self.view_button.clicked.connect(self.structure_view)
        self.save_poscar_button.clicked.connect(self.save_poscar)
        self.sort_x_button.clicked.connect(lambda: self.sort_atoms_by_axis(0))
        self.sort_y_button.clicked.connect(lambda: self.sort_atoms_by_axis(1))
        self.sort_z_button.clicked.connect(lambda: self.sort_atoms_by_axis(2))
        self.sort_button.clicked.connect(self.sort_ions)
        self.incar_option_apply_button.clicked.connect(self.apply_incar_value)
        self.reset_coordinate_button.clicked.connect(self.reset_coordinate)

        self.z_value_apply.clicked.connect(self.fix_atoms)
        self.z_value.returnPressed.connect(self.fix_atoms)
        
        self.POSCAR.dragEnterEvent = self.poscar_drag_enter_event
        self.POSCAR.dropEvent = self.poscar_drop_event

        self.kpoints_apply_button.clicked.connect(self.write_kpoints)
        self.kpoints_resolution.returnPressed.connect(self.write_kpoints)

        self.file_convert_label.dragEnterEvent = self.convert_drag_enter_event
        self.file_convert_label.dropEvent = self.convert_drop_event
        

    def apply_incar_value(self):
        self.textbox_incar.clear()
        calculation_type = self.calculation_type_comoboBox.currentText()
        incar = {
            "# General" : 1,
            "PREC" : "Accurate",
            "SYMPREC" : "1E-8",
            "ISTART" : "0",
            "ICHARG" : "2",
            "ISPIN" : "2",
            "GGA" : "",
            "IVDW" : "",
            "MAGMOM" : "",

            "# Write Flags" : None,
            "LWAVE" : ".FALSE.",
            "LCHARG" : ".FALSE.",
            "LAECHG" : ".FALSE.",
            "LVHAR" : "",
            "LORBIT" : "11",

            "# Dipole Correction" : None,
            "LDIPOL" : "",
            "IDIPOL" : "",

            "# Electronic Relaxation" : None,
            "ALGO" : "Fast",
            "ENCUT" : "400",
            "EDIFF" : "1E-7",
            "NELM" : "60",
            "LREAL" : "Auto", 
            "MAXMIX" : "",

            "# Ionic Relaxation" : None,
            "IBRION" : "",
            "NSW" : "",
            "POTIM" : "",
            "ISIF" : "2",
            "EDIFFG" : "",
            "ISYM" : "0",
            "NFREE" : "",

            "# DOS" : None,
            "ISMEAR" : "0",
            "SIGMA" : "0.05",
            "EMAX"  : "",
            "EMIN"  : "",
            "NEDOS" : "",

            "# Solvation" : None,
            "LSOL" : "",
            "ISOL" : "",
            "C_MOLAR" : "",
            "R_ION" : "",
            "EFERMI_ref" : "",

            "# DFT+U" : None,
            "LDAU" : "",
            "LDAUTYPE" : "",
            "LDAUL" : "",
            "LDAUU" : "",
            "LDAUJ" : "",
            "LMAXMIX" : "",

            "# NEB" : None,
            "IMAGES" : "",
            "IOPT"   : "",
            "ICHAIN" : "",
            "LCLIMB" : "",
            "TIMESTEP" : "",

            "# HSE" : None,
            "LHFCALC" : "",
            "HFSCREEN" : "",
            "TIME" : "",

            "# Parallelization": None,
            "LPLANE" : ".TRUE.",
            "NCORE" : "1"
        }
        
        #Magnetic Moments
        lines = self.textbox_poscar.toPlainText().splitlines()
        try:
            fields = lines[6].split()
        except IndexError:
            fields = []
        
        if fields:
            magmom = " ".join(f + "*0" for f in fields)
            incar["MAGMOM"] = magmom
        else:
            pass


        if calculation_type in ["HSE"]:
            incar["ISTART"] = "0"
        else:
            incar["ISTART"] = "0"

        if calculation_type == "HSE":
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
        

        if self.comboBox_1.currentText() == "Surface":
            if calculation_type != "Solvation":
                incar["LDIPOL"] = ".TRUE."
                incar["IDIPOL"] = "3"
            incar["LVHAR"] = ".TRUE."
            incar["ISIF"] = "2"
            incar["ISYM"] = "0"
        elif self.comboBox_1.currentText() == "Bulk":
            incar["ISIF"] = "3"
            incar["ISYM"] = "2"
        else :
            pass
        
        if self.comboBox_1.currentText() == "Molecule":
            incar["ALGO"] = "Normal"
            incar["LDIPOL"] = ".TRUE."
            incar["IDIPOL"] = "4"
        elif calculation_type == "HSE":
            incar["ALGO"] = "Damped"
        else:
            incar["ALGO"] = "Fast"
        
        cut_off_energy_value = self.cut_off_energy.value()
        incar["ENCUT"] = cut_off_energy_value

        if calculation_type == "Vibration":
            incar["MAXMIX"] = "60"
        else:
            pass
        
        if calculation_type == "Relaxation":
            incar["IBRION"] = "2"
            incar["NSW"] = "2000"
            incar["POTIM"] = "0.5"
            incar["EDIFFG"] = "1E-5"
            incar["EDIFF"] = "1E-6"
        elif calculation_type == "Vibration":
            incar["IBRION"] = "5"
            incar["NSW"] = "2000"
            incar["POTIM"] = "0.015"
            incar["EDIFFG"] = "1E-6"
            incar["NFREE"] = "2"
        elif calculation_type == "NEB":
            incar["IOPT"] = "3"
            incar["ICHAIN"] = "0"
            incar["IMAGES"] = "3"
            incar["LCLIMB"] = ".TRUE."
            incar["TIMESTEP"] = "0.05"
            incar["IBRION"] = "3"
            incar["NSW"] = "2000"
            incar["POTIM"] = "0"
            incar["EDIFFG"] = "-0.05"
        else:
            incar["IBRION"] = ""
            incar["NSW"] = "0"



        if calculation_type == "Solvation":
            incar["IBRION"] = "2"
            incar["NSW"] = "1000"
            incar["POTIM"] = "0.5"
            incar["EDIFFG"] = "1E-5"
            incar["EDIFF"] = "1E-6"
            incar["LSOL"] = ".TRUE."
            incar["ISOL"] = "2"
            incar["C_MOLAR"] = "0.01"
            incar["R_ION"] = "3"
        else:
            pass
        


        if calculation_type == "HSE":
            incar["LHFCALC"] = ".TRUE."
            incar["HFSCREEN"] = "0.2"
            incar["TIME"] = "0.4"
        else:
            pass
        

        if self.CheckBox_01.isChecked():
            lines = self.textbox_poscar.toPlainText().splitlines()

            if len(lines) >= 7:
                num_elements = len(lines[6].split())  # 7번째 줄 (인덱스 6)을 공백 기준으로 분할
            else:
                num_elements = 1  # 7번째 줄이 없으면 0

            incar["LDAU"] = ".TRUE."
            incar["LDAUTYPE"] = "2"
            incar["LDAUL"] = " ".join(["2"] * num_elements)
            incar["LDAUU"] = " ".join(["4.0"] * num_elements)
            incar["LDAUJ"] = " ".join(["0.0"] * num_elements)
            incar["LMAXMIX"] = "4"
        else:
            pass


        if self.CheckBox_02.isChecked():
            incar["LWAVE"] = ".TRUE."
        else:
            pass

        if self.CheckBox_03.isChecked():
            incar["LCHARG"] = ".TRUE."
            incar["LAECHG"] = ".TRUE."
        else:
            pass

        if self.CheckBox_04.isChecked():
            incar["EMAX"] = "20"
            incar["EMIN"] = "-40"
            incar["NEDOS"] = "6000"
        else:
            pass

        if self.CheckBox_05.isChecked():
            incar["EFERMI_ref"] = "-4.57"
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
                incar_text += f"{key.ljust(10)} = {incar[key]}\n"
        
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

    def create_message_box(self, icon, title, text, informative_text=""):
        message_box = QMessageBox(self)
        message_box.setIcon(icon)
        message_box.setWindowTitle(title)
        message_box.setText(text)

        if informative_text:
            message_box.setInformativeText(informative_text)

        return message_box

    def show_warning(self, title, text, informative_text=""):
        message_box = self.create_message_box(QMessageBox.Warning, title, text, informative_text)
        message_box.setStandardButtons(QMessageBox.Ok)
        message_box.exec_()

    def show_information(self, title, text, informative_text=""):
        message_box = self.create_message_box(QMessageBox.Information, title, text, informative_text)
        message_box.setStandardButtons(QMessageBox.Ok)
        message_box.exec_()

    def set_poscar_from_atoms(self, atoms, sort_atoms=True):
        output = StringIO()
        write(output, atoms, format='vasp', direct=False, sort=sort_atoms)
        self.textbox_poscar.clear()
        self.textbox_poscar.setPlainText(output.getvalue())

    def load_text_file_to_poscar(self, file_path):
        with open(file_path, 'r', encoding='utf-8', errors='replace') as file:
            content = file.read()
        self.textbox_poscar.clear()
        self.textbox_poscar.insertPlainText(content)

    def drop_for_read_poscar(self, event):
        for url in event.mimeData().urls():
            file_path = url.toLocalFile()
            clean_path = file_path.strip('{}')
            ext = os.path.splitext(clean_path)[-1].lower()

            if ext == '.cif':
                try:
                    atoms = read(clean_path, format='cif')
                    self.set_poscar_from_atoms(atoms)
                except Exception as e:
                    self.textbox_poscar.clear()
                    self.textbox_poscar.insertPlainText(f"Error reading structured file: {e}")
            elif ext == '.vasp':
                try:
                    self.load_text_file_to_poscar(clean_path)
                except Exception as e:
                    self.textbox_poscar.clear()
                    self.textbox_poscar.insertPlainText(f"Error reading structured file: {e}")
            else:
                try:
                    self.load_text_file_to_poscar(clean_path)
                except Exception as e:
                    self.textbox_poscar.clear()
                    self.textbox_poscar.insertPlainText(f"Error reading text file: {e}")


    def read_poscar(self):
        global Recent_used_directory
        filename, _ = QFileDialog.getOpenFileName(
            self,
            "Open Structure File",
            Recent_used_directory,
            "Structure files (*.vasp *.cif);;VASP files (*.vasp);;CIF files (*.cif);;All files (*.*)",
        )
        if filename:
            if filename.lower().endswith('.cif'):
                atoms = read(filename, format='cif')
                self.set_poscar_from_atoms(atoms)
            else:
                self.load_text_file_to_poscar(filename)
            Recent_used_directory = os.path.dirname(filename)

    def fix_atoms(self):
        lines = self.textbox_poscar.toPlainText().splitlines()
        number_of_each_atoms = lines[6].split()
        total_num_of_atom = sum([int(num) for num in number_of_each_atoms])

        if len(lines) > 7 and lines[7].lower().startswith("s" or "S"):
            lines[7] = "Selective dynamics"
        else:
            lines.insert(7, "Selective dynamics")

        input_value = float(self.z_value.text())
        position_range = total_num_of_atom + 9

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


    def sort_atoms_by_axis(self, axis_index):
        atom_group_sizes = self.get_atom_group_sizes()
        read_textbox_poscar = self.textbox_poscar.toPlainText()
        temp_vasp_type_file = StringIO(read_textbox_poscar)
        temp_ase = read(temp_vasp_type_file, format='vasp')
        axis_positions = temp_ase.get_positions()[:, axis_index]

        if not atom_group_sizes or sum(atom_group_sizes) != len(temp_ase):
            self.show_warning("Error", "Could not identify the current atom groups in POSCAR.")
            return

        sorted_indices = []
        start_index = 0

        for group_size in atom_group_sizes:
            group_indices = list(range(start_index, start_index + group_size))
            sorted_indices.extend(sorted(group_indices, key=lambda idx: axis_positions[idx]))
            start_index += group_size

        sorted_atoms = temp_ase[sorted_indices]
        sorted_poscar = StringIO()
        write(sorted_poscar, sorted_atoms, format='vasp', sort=False)
        sorted_poscar_content = sorted_poscar.getvalue()
        self.textbox_poscar.clear()
        self.textbox_poscar.setPlainText(sorted_poscar_content)

    def get_atom_group_sizes(self):
        lines = self.textbox_poscar.toPlainText().splitlines()

        for line_index in [6, 5]:
            if len(lines) <= line_index:
                continue

            try:
                atom_group_sizes = [int(num) for num in lines[line_index].split()]
            except ValueError:
                continue

            if atom_group_sizes:
                return atom_group_sizes

        return None

    def sort_ions(self):
        read_textbox_poscar = self.textbox_poscar.toPlainText()
        temp_vasp_type_file = StringIO(read_textbox_poscar)
        temp_ase = read(temp_vasp_type_file, format='vasp')
        chemical_symbols = temp_ase.get_chemical_symbols()
        symbol_order = {}

        for symbol in chemical_symbols:
            if symbol not in symbol_order:
                symbol_order[symbol] = len(symbol_order)

        sorted_indices = sorted(
            range(len(temp_ase)),
            key=lambda idx: (symbol_order[chemical_symbols[idx]], idx)
        )
        sorted_atoms = temp_ase[sorted_indices]
        sorted_poscar = StringIO()
        write(sorted_poscar, sorted_atoms, format='vasp', sort=False)
        sorted_poscar_content = sorted_poscar.getvalue()
        self.textbox_poscar.clear()
        self.textbox_poscar.setPlainText(sorted_poscar_content)

    def reset_coordinate(self):
        vasp_text = self.textbox_poscar.toPlainText()

        ase_structure = read(StringIO(vasp_text), format='vasp')
        
        ase_structure.set_pbc(True)  
        ase_structure.wrap()         
        vasp_reset_text = StringIO()
        write(vasp_reset_text, ase_structure, format='vasp')
    
        self.textbox_poscar.clear()
        self.textbox_poscar.setPlainText(vasp_reset_text.getvalue())


    def save_poscar(self):
        global Recent_used_directory
        file_path, _ = QFileDialog.getSaveFileName(self, "Save POSCAR File", Recent_used_directory, "VASP files (*.vasp);;CIF files (*.cif);;PDB files (*.pdb);;XYZ files (*.xyz);;All files (*.*)")
        if file_path:
            content = self.textbox_poscar.toPlainText()
            string_io = StringIO(content)
            atoms = read(string_io, format='vasp')
            write(file_path, atoms)


    def structure_view(self):
        structure_content = self.textbox_poscar.toPlainText().strip()

        if structure_content:
            initial = read(StringIO(structure_content), format='vasp')
        else:
            initial = Atoms()

        gui = GUI(images=[initial])
        gui.run()

        final = gui.atoms

        if len(final) == 0:
            self.textbox_poscar.clear()
            return

        output = StringIO()
        write(output, final, format='vasp')
        vasp_content = output.getvalue()
        self.textbox_poscar.clear()
        self.textbox_poscar.insertPlainText(vasp_content)
        

    def write_kpoints(self):
        input_value = float(self.kpoints_resolution.text())
        
        if input_value < 0:
            self.show_warning("Error", "K-Spacing Value must be greater than 0.")
            return
        elif input_value == 0:
            kpoints_x = 1
            kpoints_y = 1
            kpoints_z = 1
        else:
            lattcie_parameter_x = self.textbox_poscar.toPlainText().splitlines()[2].split()
            lattcie_parameter_y = self.textbox_poscar.toPlainText().splitlines()[3].split()
            lattcie_parameter_z = self.textbox_poscar.toPlainText().splitlines()[4].split()
            kpoints_x = round(1 / (math.sqrt(float(lattcie_parameter_x[0])**2 + float(lattcie_parameter_x[1])**2 + float(lattcie_parameter_x[2])**2)) / input_value)
            kpoints_y = round(1 / (math.sqrt(float(lattcie_parameter_y[0])**2 + float(lattcie_parameter_y[1])**2 + float(lattcie_parameter_y[2])**2)) / input_value)
            kpoints_z = round(1 / (math.sqrt(float(lattcie_parameter_z[0])**2 + float(lattcie_parameter_z[1])**2 + float(lattcie_parameter_z[2])**2)) / input_value)

        kpoints = (
            f"K-Spacing Value: {input_value}\n"
            f"0\n"
            f"Gamma\n"
            f"  {kpoints_x}   {kpoints_y}   {kpoints_z}\n"
            f"  0   0   0"
        )
        self.textbox_kpoints.setPlainText(kpoints)


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
            self.show_information("Saved", f"File saved as\n{save_path}")


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
