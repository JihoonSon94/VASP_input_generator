from ase.build import fcc111, fcc100, add_adsorbate
from ase.io import write

# (211) 면의 구조 생성
fcc211_structure = fcc111('Al', size=(2, 2, 5), a=4.05)
fcc211_structure.rotate(45, 'z', center='cop', rotate_cell=True)

# 왼쪽에 (111) 면 나열
fcc111_structure_left = fcc111('Al', size=(2, 2, 5), a=4.05)
fcc111_structure_left.translate((-fcc211_structure.get_cell()[0, 0], 0, 0))

# 오른쪽에 (100) 면 나열
fcc100_structure_right = fcc100('Al', size=(2, 2, 5), vacuum=10.0, orthogonal=True)
fcc100_structure_right.translate((fcc211_structure.get_cell()[0, 0], 0, 0))

# (211) 면과 (111), (100) 면을 합치기
edge_structure = fcc211_structure + fcc111_structure_left + fcc100_structure_right


# CIF 파일로 저장
write('fcc_edge_structure.cif', edge_structure)
