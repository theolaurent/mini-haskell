
open Mips
open Ir
(* TODO : check difference between jump and branch *)

(* TODO : question : values are alway in the heap or can be on the stack (block of several words)
                     this will determine if we can store a value in a register ... *)

(* value are represented as in https://www.lri.fr/~filliatr/ens/compil/examen/janvier-12.pdf *)
(* TODO : optim : store tags on only 1 byte *)

(* boolean false is 0 *)

(* current environement is pointed by register ... *)

let compile_label (L l) = ("L" ^ string_of_int l)


(* create the value v on the heap and put its address in v0 *)
(* be sareful it modifies a0 and a lot of others *)
let compile_value v = match v with
  | Imm i ->
     li a0 8 ++ comment "enough space for a tag + an integer"
  ++ li v0 9 ++ comment "syscall 9 (sbrk)"
  ++ syscall
  ++ comment "address of the allocated space is now in $v0"
  ++ li a1 0 ++ comment "0 is the tag for integers"
  ++ sw a1 areg (0, v0) ++ comment "store the tag"
  ++ li a2 i
  ++ sw a2 areg (4, v0) ++ comment "store the immediate value"
  | Cons (v1, v2) -> failwith "TODO"
  | Clos (l, env) -> failwith "TODO"
  | Froz (l, env) -> failwith "TODO"

let compile_instr ir = match ir with
  | Force -> failwith "TODO"
  | Label l ->
     label (compile_label l)
  | Value v ->
     compile_value v (* modify registers but neither sp nor ra *)
  | Branch l ->
     b (compile_label l)
  | BranchFalse l ->
     beqz v0 (compile_label l)
  | CallFun -> failwith "TODO"
  | ReturnCall -> failwith "TODO"
  | ReturnForce -> failwith "TODO"
  | Fetch i -> failwith "TODO"
  | Store i -> failwith "TODO"
