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

    def initUI(self):
        self.setWindowTitle("INCAR Generator")
        self.setGeometry(500, 100, 800, 800)
        self.setFixedSize(800, 800)

        self.tabs = QTabWidget(self)
        self.tabs.setGeometry(10, 10, 780, 780)

        self.tab1 = QWidget()
        self.tab2 = QWidget()
        self.tab3 = QWidget()
        self.tab4 = QWidget()
        self.tab5 = QWidget()
        self.tab6 = QWidget()

        self.tabs.addTab(self.tab1, "INCAR")
        self.tabs.addTab(self.tab2, "POSCAR")
        self.tabs.addTab(self.tab3, "KPOINTS")
        self.tabs.addTab(self.tab4, "Shell")
        self.tabs.addTab(self.tab5, "File Convertor")
        self.tabs.addTab(self.tab6, "Visualization")

        self.initTab1()
        self.initTab2()
        self.initTab3()
        self.initTab4()
        self.initTab5()
        self.initTab6()

    def initTab1(self):
        layout = QVBoxLayout()

        self.text1 = QTextEdit(self.tab1)
        layout.addWidget(self.text1)

        model_label = QLabel("Model Type:", self.tab1)
        layout.addWidget(model_label)
        self.model_combobox = QComboBox(self.tab1)
        self.model_combobox.addItems(["Bulk", "Surface", "Molecule"])
        layout.addWidget(self.model_combobox)

        calculation_label = QLabel("Calculation Type:", self.tab1)
        layout.addWidget(calculation_label)
        self.calculation_combobox = QComboBox(self.tab1)
        self.calculation_combobox.addItems(["Relaxation", "Single Point", "Charge", "Vibration", "DOS", "HSE", "Solvation"])
        layout.addWidget(self.calculation_combobox)

        functional_label = QLabel("Functional:", self.tab1)
        layout.addWidget(functional_label)
        self.functional_combobox = QComboBox(self.tab1)
        self.functional_combobox.addItems(["PBE", "RPBE", "PW91"])
        layout.addWidget(self.functional_combobox)

        cutoff_label = QLabel("Cut-off Energy:", self.tab1)
        layout.addWidget(cutoff_label)
        self.cutoff_combobox = QComboBox(self.tab1)
        self.cutoff_combobox.addItems(["400", "500", "600", "700"])
        layout.addWidget(self.cutoff_combobox)

        vdw_label = QLabel("VDW Correction:", self.tab1)
        layout.addWidget(vdw_label)
        self.vdw_combobox = QComboBox(self.tab1)
        self.vdw_combobox.addItems(["None", "D2", "D3"])
        layout.addWidget(self.vdw_combobox)

        self.apply_button = QPushButton("Apply", self.tab1)
        self.apply_button.clicked.connect(self.apply_incar_value)
        layout.addWidget(self.apply_button)

        self.tab1.setLayout(layout)

    def initTab2(self):
        layout = QVBoxLayout()

        self.text2 = QTextEdit(self.tab2)
        layout.addWidget(self.text2)

        self.import_button = QPushButton("Import", self.tab2)
        self.import_button.clicked.connect(self.read_poscar)
        layout.addWidget(self.import_button)

        z_label = QLabel("Z value:", self.tab2)
        layout.addWidget(z_label)
        self.z_value_entry = QLineEdit(self.tab2)
        layout.addWidget(self.z_value_entry)

        self.z_value_apply_btn = QPushButton("Apply value", self.tab2)
        self.z_value_apply_btn.clicked.connect(self.fix_atoms)
        layout.addWidget(self.z_value_apply_btn)

        self.save_poscar_btn = QPushButton("Save as File", self.tab2)
        self.save_poscar_btn.clicked.connect(self.write_poscar)
        layout.addWidget(self.save_poscar_btn)

        self.view_structure_btn = QPushButton("View", self.tab2)
        self.view_structure_btn.clicked.connect(self.structure_view)
        layout.addWidget(self.view_structure_btn)

        self.tab2.setLayout(layout)

    def initTab3(self):
        layout = QVBoxLayout()

        self.text3 = QTextEdit(self.tab3)
        layout.addWidget(self.text3)

        model_label = QLabel("Model Type:", self.tab3)
        layout.addWidget(model_label)
        self.kpoints_combobox = QComboBox(self.tab3)
        self.kpoints_combobox.addItems(["Bulk", "Surface", "Molecule"])
        layout.addWidget(self.kpoints_combobox)

        self.kpoints_apply_btn = QPushButton("Apply", self.tab3)
        self.kpoints_apply_btn.clicked.connect(self.write_kpoints)
        layout.addWidget(self.kpoints_apply_btn)

        self.tab3.setLayout(layout)

    def initTab4(self):
        layout = QVBoxLayout()

        self.shell_text = QTextEdit(self.tab4)
        layout.addWidget(self.shell_text)

        server_label = QLabel("Select Server:", self.tab4)
        layout.addWidget(server_label)
        self.shell_combobox = QComboBox(self.tab4)
        self.shell_combobox.addItems(["CEMD"])
        layout.addWidget(self.shell_combobox)

        self.shell_apply_btn = QPushButton("Apply", self.tab4)
        self.shell_apply_btn.clicked.connect(self.make_shell)
        layout.addWidget(self.shell_apply_btn)

        self.tab4.setLayout(layout)

    def initTab5(self):
        layout = QVBoxLayout()

        self.drop_label = QLabel("Drag and drop a .cif or .vasp file here", self.tab5)
        layout.addWidget(self.drop_label)

        self.tab5.setAcceptDrops(True)
        self.tab5.dragEnterEvent = self.dragEnterEvent
        self.tab5.dropEvent = self.drop_for_file_convert

        self.tab5.setLayout(layout)

    def initTab6(self):
        layout = QVBoxLayout()

        self.visualization_import_btn = QPushButton("Import", self.tab6)
        self.visualization_import_btn.clicked.connect(self.visualization)
        layout.addWidget(self.visualization_import_btn)

        self.drop_label_visualization = QLabel("Drag and drop a file here", self.tab6)
        layout.addWidget(self.drop_label_visualization)

        self.tab6.setAcceptDrops(True)
        self.tab6.dragEnterEvent = self.dragEnterEvent
        self.tab6.dropEvent = self.visualization_drop

        self.tab6.setLayout(layout)

    def apply_incar_value(self):
        self.text1.clear()
        # General
        self.text1.
        # Implementation for applying INCAR values goes here

    def read_poscar(self):
        global Recent_used_directory
        filename, _ = QFileDialog.getOpenFileName(self, "Open POSCAR File", Recent_used_directory, "VASP files (*.vasp);;All files (*.*)")
        if filename:
            with open(filename, 'r') as file:
                file_content = file.read()
            self.text2.setText(file_content)
            Recent_used_directory = os.path.dirname(filename)

    def fix_atoms(self):
        # Implementation for fixing atoms based on Z value
        pass

    def write_poscar(self):
        global Recent_used_directory
        file_path, _ = QFileDialog.getSaveFileName(self, "Save POSCAR File", Recent_used_directory, "VASP files (*.vasp);;All files (*.*)")
        if file_path:
            content = self.text2.toPlainText()
            string_io = StringIO(content)
            atoms = read(string_io, format='vasp')
            write(file_path, atoms)

    def structure_view(self):
        structure_content = self.text2.toPlainText()
        structure_io = StringIO(structure_content)
        visualize = read(structure_io, format='vasp')
        view(visualize)

    def write_kpoints(self):
        # Implementation for writing k-points
        pass

    def make_shell(self):
        # Implementation for generating shell script
        pass

    def visualization(self):
        filename, _ = QFileDialog.getOpenFileName(self, "Open File for Visualization", Recent_used_directory, "VASP files (*.vasp);;All files (*.*)")
        if filename:
            visualize = read(filename)
            view(visualize)

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

    def visualization_drop(self, event: QDropEvent):
        for url in event.mimeData().urls():
            file_path = url.toLocalFile()
            visualize = read(file_path)
            view(visualize)

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