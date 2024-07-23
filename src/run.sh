ocamlopt -c vector.mli heap.mli
ocamlopt -O3 vector.ml heap.ml rubiks_cube_3x3.ml
./a.out > "transfert.txt"
