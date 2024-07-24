from tkinter import *
from tkinter import filedialog
from tkinter import ttk
from tkinter import scrolledtext
from tkinterdnd2 import TkinterDnD, DND_FILES
from ase.io import read, write
from ase.atoms import Atoms
import math
import os

Desktop_directory = os.environ['USERPROFILE']  + "\Desktop"
Recent_used_directory=Desktop_directory

def apply_incar_value():
    text1.delete(1.0, END)

    #################### General ####################
    text1.insert(CURRENT, "#General\n")
    text1.insert(CURRENT, "PREC    = Accurate\n")
    text1.insert(CURRENT, "SYMPREC = 1E-8\n")
    if combobox_calculation_type.get() == "Solvation":
        text1.insert(CURRENT, "ISTART  = 1\n")
        text1.insert(CURRENT, "ICHARG  = 2\n")
    elif combobox_calculation_type.get() == "HSE":
        text1.insert(CURRENT, "ISTART  = 1\n")
        text1.insert(CURRENT, "ICHARG  = 11\n")
    else:
        text1.insert(CURRENT, "ISTART  = 0\n")
        text1.insert(CURRENT, "ICHARG  = 2\n")
    text1.insert(CURRENT, "ISPIN   = 2\n")
    text1.insert(CURRENT, "\n")

    #################### Write flags ####################
    text1.insert(CURRENT, "#Write flags\n")

    calculation_type = combobox_calculation_type.get()
    if calculation_type in ["Single Point", "Vibration", "Charge", "DOS", "HSE"]:
        text1.insert(CURRENT, "LORBIT  = 11\n")
    else:
        pass

    if combobox_calculation_type.get() == "Single Point":
        text1.insert(CURRENT, "LWAVE   = .TRUE.\n")
        text1.insert(CURRENT, "LCHARG  = .TRUE.\n")
    elif combobox_calculation_type.get() == "Solvation":
        text1.insert(CURRENT, "LWAVE   = .FALSE.\n")
        text1.insert(CURRENT, "LCHARG  = .FALSE.\n")
    elif combobox_calculation_type.get() == "Charge":
        text1.insert(CURRENT, "LWAVE   = .FALSE.\n")
        text1.insert(CURRENT, "LCHARG  = .TRUE.\n")
        text1.insert(CURRENT, "LAECHG  = .TRUE.\n")
    elif combobox_calculation_type.get() == "DOS":
        text1.insert(CURRENT, "LWAVE   = .TRUE.\n")
        text1.insert(CURRENT, "LVTOT   = .TRUE.\n")
        text1.insert(CURRENT, "LCHARG  = .TRUE.\n")
        text1.insert(CURRENT, "LAECHG  = .TRUE.\n")
    else:
        text1.insert(CURRENT, "LWAVE   = .FALSE.\n")
        text1.insert(CURRENT, "LCHARG  = .FALSE.\n")
    text1.insert(CURRENT, "\n")

    #################### Exchange Correlation ####################
    text1.insert(CURRENT, "#Exchange Correlation\n")
    if combobox3.get() == "PBE":
        text1.insert(CURRENT, "GGA     = PE\n")
        text1.insert(CURRENT, "\n")
    elif combobox3.get() == "RPBE":
        text1.insert(CURRENT, "GGA     = RP\n")
        text1.insert(CURRENT, "\n")
    elif combobox3.get() == "PW91":
        text1.insert(CURRENT, "GGA     = 91\n")
        text1.insert(CURRENT, "\n")
    else:
        text1.insert(CURRENT, "\n")

    #################### Dipole Correction ####################
    text1.insert(CURRENT, "#Dipole Correction\n")
    if combobox_model_type.get() == "Surface":
        text1.insert(CURRENT, "LDIPOL  = .TRUE.\n")
        text1.insert(CURRENT, "IDIPOL  = 3\n")
        text1.insert(CURRENT, "\n")
    else:
        text1.insert(CURRENT, "\n")

    #################### Van der Waals Correction ####################
    text1.insert(CURRENT, "#Van der Waals Correction\n")
    if combobox5.get() == "D2":
        text1.insert(CURRENT, "IVDW    = 1\n")
        text1.insert(CURRENT, "\n")
    elif combobox5.get() == "D3":
        text1.insert(CURRENT, "IVDW    = 11\n")
        text1.insert(CURRENT, "\n")
    else:
        text1.insert(CURRENT, "\n")

    #################### Solvation Correction ####################
    if combobox_calculation_type.get() == "Solvation":
        text1.insert(CURRENT, "#Solvation Correction\n")
        text1.insert(CURRENT, "LSOL    = .TRUE.\n")
        text1.insert(CURRENT, "TAU     = 0\n")
        text1.insert(CURRENT, "\n")
    else:
        pass
    
    #################### Electronic Relaxation ####################
    text1.insert(CURRENT, "#Electronic Relaxation\n")
    if combobox_calculation_type.get() == "HSE":
        pass
    elif combobox_model_type.get() == "Molecule":
        text1.insert(CURRENT, "IALGO   = 38\n")
    else:
        text1.insert(CURRENT, "IALGO   = 48\n")

    text1.insert(CURRENT, "ENCUT   = " + str(combobox4.get()) + "\n")
    text1.insert(CURRENT, "EDIFF   = 1E-7\n")
    text1.insert(CURRENT, "NELM    = 300\n")
    text1.insert(CURRENT, "LREAL   = Auto\n")

    if combobox_calculation_type.get() == "Vibration":
        text1.insert(CURRENT, "MAXMIX  = 60\n")
        text1.insert(CURRENT, "\n")
    else:
        text1.insert(CURRENT, "\n")

    #################### Ionic Relaxation ####################
    text1.insert(CURRENT, "#Ionic Relaxation\n")
    if combobox_calculation_type.get() == "Relaxation":
        text1.insert(CURRENT, "IBRION  = 2\n")
        text1.insert(CURRENT, "NSW     = 1000\n")
        text1.insert(CURRENT, "POTIM   = 0.5\n")
        if combobox_model_type.get() == "Bulk":
            text1.insert(CURRENT, "ISIF    = 3\n")
        else:
            text1.insert(CURRENT, "ISIF    = 2\n")
        text1.insert(CURRENT, "EDIFFG  = 1E-6\n")
        text1.insert(CURRENT, "ISYM    = 0\n")
        text1.insert(CURRENT, "\n")
    elif combobox_calculation_type.get() == "Vibration":
        text1.insert(CURRENT, "IBRION  = 5\n")
        text1.insert(CURRENT, "NSW     = 1000\n")
        text1.insert(CURRENT, "POTIM   = 0.015\n")
        text1.insert(CURRENT, "ISIF    = 2\n")
        text1.insert(CURRENT, "EDIFFG  = 1E-6\n")
        text1.insert(CURRENT, "ISYM    = 0\n")
        text1.insert(CURRENT, "NFREE   = 2\n")
        text1.insert(CURRENT, "\n")
    else:
        text1.insert(CURRENT, "IBRION  = -1\n")
        text1.insert(CURRENT, "NSW     = 0\n")
        text1.insert(CURRENT, "\n")

    #################### HSE ####################
    if combobox_calculation_type.get() == "HSE":
        text1.insert(CURRENT, "#HSE06\n")
        text1.insert(CURRENT, "LHFCALC  = .TURE.\n")
        text1.insert(CURRENT, "HFSCREEN = 0.2\n")
        text1.insert(CURRENT, "ALGO     = Damped\n")
        text1.insert(CURRENT, "TIME     = 0.4\n")
        text1.insert(CURRENT, "\n")

    #################### DOS ####################
    if combobox_calculation_type.get() == "DOS":
        text1.insert(CURRENT, "#DOS\n")
        text1.insert(CURRENT, "ISMEAR  = -5\n")
        text1.insert(CURRENT, "SIGMA   = 0.05\n")
        text1.insert(CURRENT, "EMAX    = 20\n")
        text1.insert(CURRENT, "EMIN    = -20\n")
        text1.insert(CURRENT, "NEDOS   = 4000\n")
        text1.insert(CURRENT, "\n")
    else:
        text1.insert(CURRENT, "#DOS\n")
        text1.insert(CURRENT, "ISMEAR  = 0\n")
        text1.insert(CURRENT, "SIGMA   = 0.05\n")
        text1.insert(CURRENT, "\n")

    #################### Computational Parameter ####################
    text1.insert(CURRENT, "#Computational Parameter\n")
    text1.insert(CURRENT, "LPLANE  = .TRUE.\n")
    text1.insert(CURRENT, "NCORE   = 1\n")

def convert_cif2vasp(file_path):
    if file_path:
        atoms = read(file_path, format='cif')
        file_name = os.path.basename(file_path)
        file_name_without_ext = os.path.splitext(file_name)[0]
        save_path = os.path.join(os.path.dirname(file_path), file_name_without_ext + '.vasp')
        write(save_path, atoms, format='vasp', direct=True, sort=True)
        print(f"File saved as {save_path}")
    else:
        print("File selection cancelled.")

def convert_vasp2cif(file_path):
    if file_path:
        atoms = read(file_path, format='vasp')
        file_name = os.path.basename(file_path)
        file_name_without_ext = os.path.splitext(file_name)[0]
        save_path = os.path.join(os.path.dirname(file_path), file_name_without_ext + '.cif')
        write(save_path, atoms, format='cif')
        print(f"File saved as {save_path}")
    else:
        print("File selection cancelled.")

def drop(event):
    file_paths = card.tk.splitlist(event.data)
    for file_path in file_paths:
        clean_path = file_path.strip('{}')
        if file_path.lower().endswith('.cif'):
            convert_cif2vasp(clean_path)
        else:
            convert_vasp2cif(clean_path)

def read_poscar():
    global Recent_used_directory
    filename = filedialog.askopenfilename(initialdir=Recent_used_directory, filetypes=(("vasp files", "*.vasp"),("all files","*.*")))

    if filename:
        Recent_used_directory= "/".join(filename.split("/")[:-1])
    
    IN = open(filename,"r")
    
    lines = IN.readlines()
    lines = list(map(lambda s: s.strip(), lines))

    text2.delete(1.0, END)

    for i in range(0,2):
        text2.insert(CURRENT, lines[i] + "\n")
    
    for i in range(2,5):
        temp = lines[i].split()
        text2.insert(CURRENT, "    ")
        for i in range(3):
            if float(temp[i]) < 0 or float(temp[i]) >= 10:
                text2.insert(CURRENT, "   ")
                text2.insert(CURRENT, "{:.16f}".format(round(float(temp[i]),16)))
            else:
                text2.insert(CURRENT, "    ")
                text2.insert(CURRENT, "{:.16f}".format(round(float(temp[i]),16)))
        text2.insert(CURRENT, "\n")
    
    for i in range(5,7):
        text2.insert(CURRENT, "    ")
        text2.insert(CURRENT, lines[i])
        text2.insert(CURRENT, "\n")

    match = str(lines[7])
    if ((match[0]) == "s") or ((match[0]) == "S"):
        text2.insert(CURRENT, "Selective dynamics\n")
        text2.insert(CURRENT, lines[8])
        text2.insert(CURRENT, "\n")
        m = 9
    else:
        text2.insert(CURRENT, lines[7])
        text2.insert(CURRENT, "\n")
        m = 8

    end = m
    number_of_each_atoms = lines[6].split()
    number_of_atom_type = int(len(number_of_each_atoms))

    count = 0
    array = []
    for i in range(0, number_of_atom_type):
        atom_coordinate = []
        pre = end
        end += int(number_of_each_atoms[i])
        count += int(number_of_each_atoms[i])
        for j in range(pre, end):
            temp = lines[j].split()
            atom_coordinate += [[temp[0], temp[1], temp[2]]]
        array += sorted(atom_coordinate, key = lambda x:x[2])
    
    for i in range(count):
        temp = array[i]
        for i in range(3):
            text2.insert(CURRENT, "  ")
            text2.insert(CURRENT, "{:.16f}".format(round(float(temp[i]),16)))
        text2.insert(CURRENT, "\n")

def fix_atoms():
    number_of_each_atoms = text2.get("7.0","7.end").split()
    total_num_of_atom=sum([int(num) for num in number_of_each_atoms])

    match = text2.get("8.0")
    if (match == "s") or (match == "S"):
        text2.delete("8.0", "8.end")
        text2.insert("8.0", "Selective dynamics")
    else:
        text2.insert("8.0", "Selective dynamics\n")
    
    input_value = entry1.get()
    position_range = total_num_of_atom + 10
    for i in range(10, position_range):
        start = str(i) + ".0"
        end = str(i) + ".end"
        atom_position_list = (text2.get(start, end)).split()
        atom_position_string = ""
        for i in range(3):
            if float(atom_position_list[i]) < 0:
                atom_position_string += " " + "{:.16f}".format(float(atom_position_list[i]))
            else:
                atom_position_string += "  " + "{:.16f}".format(float(atom_position_list[i]))
        
        text2.delete(start, end)
        if atom_position_list[2] >= input_value:
            text2.insert(start, atom_position_string)
            text2.insert(end, "   T   T   T")
        else:
            text2.insert(start, atom_position_string)
            text2.insert(end, "   F   F   F")

def write_poscar():
    global Recent_used_directory
    file_path = filedialog.asksaveasfilename(
        initialdir=Recent_used_directory,
        defaultextension=".vasp",
        filetypes=[("VASP", "*.vasp"), ("All files", "*.*")]
    )
    if file_path:
        with open(file_path, 'w') as file:
            content = text2.get(1.0, END)
            file.write(content)

def write_kpoints():
    text3.delete(1.0, END)
    text3.insert(CURRENT, "K-Points\n")
    text3.insert(CURRENT, "   0\n")
    text3.insert(CURRENT, "Gamma\n")

    kpoints = []
    
    if combobox6.get() == "Bulk":
        poscar_lattice_parametor_x = text2.get("3.0", "3.end")
        poscar_lattice_parametor_y = text2.get("4.0", "4.end")
        poscar_lattice_parametor_z = text2.get("5.0", "5.end")
        x_list = poscar_lattice_parametor_x.split()
        y_list = poscar_lattice_parametor_y.split()
        z_list = poscar_lattice_parametor_z.split()
        kpoints_x = math.ceil(40/math.sqrt(float(x_list[0])**2 + float(x_list[1])**2 + float(x_list[2])**2))
        kpoints_y = math.ceil(40/math.sqrt(float(y_list[0])**2 + float(y_list[1])**2 + float(y_list[2])**2))
        kpoints_z = math.ceil(40/math.sqrt(float(z_list[0])**2 + float(z_list[1])**2 + float(z_list[2])**2))
        kpoints.append(kpoints_x)
        kpoints.append(kpoints_y)
        kpoints.append(kpoints_z)
        text3.insert(CURRENT, "   " + str(kpoints[0]) + "   " + str(kpoints[1]) + "   " + str(kpoints[2]) + "\n")
    elif combobox6.get() == "Surface":
        poscar_lattice_parametor_x = text2.get("3.0", "3.end")
        poscar_lattice_parametor_y = text2.get("4.0", "4.end")
        poscar_lattice_parametor_z = text2.get("5.0", "5.end")
        x_list = poscar_lattice_parametor_x.split()
        y_list = poscar_lattice_parametor_y.split()
        z_list = poscar_lattice_parametor_z.split()
        kpoints_x = math.ceil(40/math.sqrt(float(x_list[0])**2 + float(x_list[1])**2 + float(x_list[2])**2))
        kpoints_y = math.ceil(40/math.sqrt(float(y_list[0])**2 + float(y_list[1])**2 + float(y_list[2])**2))
        kpoints_z = math.ceil(40/math.sqrt(float(z_list[0])**2 + float(z_list[1])**2 + float(z_list[2])**2))
        kpoints.append(kpoints_x)
        kpoints.append(kpoints_y)
        kpoints.append(kpoints_z)
        text3.insert(CURRENT, "   " + str(kpoints[0]) + "   " + str(kpoints[1]) + "   1\n")
    elif combobox6.get() == "Molecule":
        text3.insert(CURRENT, "   1   1   1\n")
    
    text3.insert(CURRENT, "   0   0   0\n")

def make_shell():
    shell_text.insert(CURRENT, "#!/bin/bash\n")
    shell_text.insert(CURRENT, "#PBS -N\n")
    shell_text.insert(CURRENT, "#PBS -q batch\n")
    shell_text.insert(CURRENT, "#PBS -l nodes=1:ppn=32\n")
    shell_text.insert(CURRENT, "#PBS -l walltime=10000:00:00\n")
    shell_text.insert(CURRENT, "#PBS -A vasp\n")
    shell_text.insert(CURRENT, "\n")
    shell_text.insert(CURRENT, "cd $PBS_O_WORKDIR\n")
    shell_text.insert(CURRENT, "mpirun /appl/programs/vasp/vasp.5.4.4/bin/vasp_std > log\n")
    shell_text.insert(CURRENT, "\n")
    shell_text.insert(CURRENT, "exit 0\n")

card = TkinterDnD.Tk()
card.geometry("750x710+500+100")
card.title("INCAR Generator")
card.resizable(0,0)

# Tab Configuration
notebook = ttk.Notebook(card, width = 730, height = 680)
notebook.pack()

tab1 = Frame(card)
notebook.add(tab1, text = "INCAR")
tab2 = Frame(card)
notebook.add(tab2, text = "POSCAR")
tab3 = Frame(card)
notebook.add(tab3, text = "KPOINTS")
tab4 = Frame(card)
notebook.add(tab4, text = "shell")
tab5 = Frame(card)
notebook.add(tab5, text = "File Convertor")

############## First Tab - INCAR ##############
## Text box
text1 = scrolledtext.ScrolledText(tab1, relief="solid", font = ("Consolas", 10))
text1.place(width = 400, height = 660, x = 10, y = 10)

## Model Type
label1 = Label(tab1, text = "Model Type :", relief = "flat", anchor = "w")
label1.place(width = 200, height = 25, x = 420, y = 10)
combobox_model_type = ttk.Combobox(tab1, values = ("Bulk", "Surface", "Molecule"))
combobox_model_type.set("Surface")
combobox_model_type.place(width = 100, height = 25, x = 620, y = 10)

## Calculation Type
label2 = Label(tab1, text="Calculation Type :", relief="flat", anchor="w")
label2.place(width = 200, height = 25, x = 420, y = 40)
combobox_calculation_type = ttk.Combobox(tab1, values = ("Relaxation", "Single Point", "Charge", "Vibration", "DOS", "HSE", "Solvation"))
combobox_calculation_type.set("Relaxation")
combobox_calculation_type.place(width = 100, height = 25, x = 620, y = 40)

## Functional
label3 = Label(tab1, text = "Functional :", relief = "flat", anchor = "w")
label3.place(width = 200, height = 25, x = 420, y = 70)
combobox3 = ttk.Combobox(tab1, values = ("PBE", "RPBE", "PW91"))
combobox3.set("PBE")
combobox3.place(width = 100, height = 25, x = 620, y = 70)

## Cut-off Energy
label4 = Label(tab1, text="Cut-off Energy :", relief="flat", anchor="w")
label4.place(width = 200, height = 25, x = 420, y = 100)
combobox4 = ttk.Combobox(tab1, values = ("400","500","600","700"))
combobox4.set("400")
combobox4.place(width = 100, height = 25, x = 620, y = 100)

## Van der Waals Correction
label5 = Label(tab1, text="VDW Correction :", relief="flat", anchor="w")
label5.place(width = 200, height = 25, x = 420, y = 130)
combobox5 = ttk.Combobox(tab1, values = ("None", "D2", "D3"))
combobox5.set("None")
combobox5.place(width = 100, height = 25, x = 620, y = 130)

## Button
btn1 = Button(tab1, text = "Apply", command = apply_incar_value)
btn1.configure(font = ("Arial", 10, "bold"), bg = "#CCCCCC")
btn1.place(width = 100, height = 30, x = 620, y = 160)

############## Second Tab - POSCAR ##############
## Express box
text2 = scrolledtext.ScrolledText(tab2, relief="solid", font = ("Consolas", 10))
text2.place(width = 550, height = 660, x = 10, y = 10)

## Import
btn3 = Button(tab2, text = "Import", command = read_poscar)
btn3.configure(font = ("Arial", 10, "bold"), bg = "#CCCCCC")
btn3.place(width = 100, height = 30, x = 620, y = 10)

## Z-value
label6 = Label(tab2, text="Z value :", relief="flat", anchor="w")
label6.place(width = 70, height = 30, x = 560, y = 50)

entry1 = Entry(tab2, border = 1, relief = "solid", font = ("Consolas", 10))
entry1.place(width = 100, height = 30, x = 620, y = 50)

btn4 = Button(tab2, text = "Apply value", command = fix_atoms)
btn4.configure(font = ("Arial", 10, "bold"), bg = "#CCCCCC")
btn4.place(width = 100, height = 30, x = 620, y = 90)

## Write POSCAR file
Make_POSCAR_btn = Button(tab2, text = "Save as File", command = write_poscar)
Make_POSCAR_btn.configure(font = ("Arial", 10, "bold"), bg = "#CCCCCC")
Make_POSCAR_btn.place(width = 100, height= 30, x = 620, y=200)

############## Third Tab - K-points ##############
## text box
text3 = scrolledtext.ScrolledText(tab3, relief="solid", font = ("Consolas", 10))
text3.place(width = 520, height = 660, x = 10, y = 10)

## Model Type
label7 = Label(tab3, text = "Model Type :", relief = "flat", anchor = "w")
label7.place(width = 200, height = 30, x = 540, y = 10)

combobox6 = ttk.Combobox(tab3, values = ("Bulk", "Surface", "Molecule"))
combobox6.set("Surface")
combobox6.place(width = 100, height = 30, x = 620, y = 10)

btn6 = Button(tab3, text = "Apply", command = write_kpoints)
btn6.configure(font = ("Arial", 10, "bold"), bg = "#CCCCCC")
btn6.place(width = 100, height = 30, x = 620, y = 50)

############## Forth Tab - Shell script ##############
## text box
shell_text = scrolledtext.ScrolledText(tab4, relief="solid", font = ("Consolas", 10))
shell_text.place(width = 520, height = 660, x = 10, y = 10)

## Model Type
shell_label = Label(tab4, text = "Select Sever :", relief = "flat", anchor = "w")
shell_label.place(width = 200, height = 30, x = 540, y = 10)

shell_combobox = ttk.Combobox(tab4, values = ("CEMD"))
shell_combobox.set("CEMD")
shell_combobox.place(width = 100, height = 30, x = 620, y = 10)

shell_btn = Button(tab4, text = "Apply", command = make_shell)
shell_btn.configure(font = ("Arial", 10, "bold"), bg = "#CCCCCC")
shell_btn.place(width = 100, height = 30, x = 620, y = 50)


############## Fifth Tab - File Convertor ##############
drop_frame = Frame(tab5, bd=2, relief="sunken", width=500, height=500)
drop_frame.pack(padx=10, pady=10, expand=True)

drop_label = Label(drop_frame, text="Drag and drop a .cif or .vasp file here", width=40, height=10)
drop_label.pack(padx=10, pady=10, expand=True)

# 드래그 앤 드롭 이벤트를 바인딩합니다.
drop_frame.drop_target_register(DND_FILES)
drop_frame.dnd_bind('<<Drop>>', drop)

card.mainloop()