
open Mips
open Ir

module Tag =
  struct
    let int = 0
    let cons = 1
    let closure = 2
    let frozen = 3
    let unfrozen = 4
  end

(* TODO : check difference between jump and branch *)

(* TODO : question : values are alway in the heap or can be on the stack (block of several words)
                     this will determine if we can store a value in a register ... *)

(* value are represented as in https://www.lri.fr/~filliatr/ens/compil/examen/janvier-12.pdf *)
(* TODO : optim : store tags on only 1 byte *)

(* boolean false is 0 *)

(* s0 and s1 should always be restored (they contains the current argument and closure respectively *)

(* current environement is pointed by register ... *)

let compile_label (L l) = ("L" ^ string_of_int l)

(* force applies forcing algorithm on value pointed by v0 *)
let force =
  label "force"
  ++ lw t0 areg (0, v0)
  ++ li t1 Tag.frozen    ++ comment "frozen tag"
  ++ bge t0 t1 "force_frz"
  ++ jr ra
  ++ label "force_frz"
  ++ beq t0 t1 "force_frozen"
  ++ lw v0 areg (4, v0)
  ++ jr ra
  ++ label "force_frozen"
  ++ push ra
  ++ push v0
  ++ lw a1 areg (4, v0) ++ comment "load the closure"
  ++ lw t0 areg (4, a1) ++ comment "load the code ptr"
  ++ jalr t0            ++ comment "evaluate function"
  ++ jal "force"        ++ comment "recursively apply force on the result"
  ++ comment "At this point, the result of this force is in v0"
  ++ pop t0             ++ comment "restore forced value in t0"
  ++ li t1 Tag.unfrozen ++ comment "unfrozen tag"
  ++ sw t1 areg (0, t0) ++ comment "update tag"
  ++ sw v0 areg (4, t0) ++ comment "update the value"
  ++ pop ra
  ++ jr ra

let exit_code code =
  li a0 code
  ++ li v0 17
  ++ syscall

let error =
  comment "error primtive"
  ++ move a1 a0
  ++ la a0 alab "error_msg"
  ++ li v0 4
  ++ syscall            ++ comment "print string"
  ++ lw t0 areg (0, a1) ++ comment "load the tag of the list"
  ++ j "end_loop_error"
  ++ label "start_loop_error"
  ++ lw v0 areg (4, a1)
  ++ jal "force"
  ++ lw a0 areg (4, v0) ++ comment "load the head"
  ++ li v0 11
  ++ syscall            ++ comment "print char"
  ++ lw v0 areg (8, a1) ++ comment "load the tail"
  ++ jal "force"
  ++ move a1 v0
  ++ lw t0 areg (0, a1)
  ++ label "end_loop_error"
  ++ li t1 Tag.cons     ++ comment "tag of cons"
  ++ beq t0 t1 "start_loop_error"
  ++ exit_code 1


let putChar =
  comment "putChar primitive"
  ++ lw a0 areg (4, a0)
  ++ li v0 11
  ++ syscall

let divC d r1 r2 =
  bnez r2 "divC"
  ++ la t0 alab "div_by_zero"
  ++ li v0 4
  ++ syscall
  ++ exit_code 1
  ++ label "divC"
  ++ div d r1 oreg r2

let remC d r1 r2 =
  bnez r2 "remC"
  ++ la t0 alab "div_by_zero"
  ++ li v0 4
  ++ syscall
  ++ exit_code 1
  ++ label "remC"
  ++ rem d r1 oreg r2


let error_msg =
  label "error_msg"
  ++ asciiz "error : "

let div_by_zero_msg =
  label "div_by_zero"
  ++ asciiz "error : division by zero\n"


(* Store the env in memory [ *r + delta ; *r + delta + 4 * (length env - 1)]
   r must be different from a1, sp, ra
   I've assumed the environment starts at sp but I'm probably wrong...
 *)
let read_variable dest var =
  match var with
  | GlobalVar s -> lw dest alab ("G" ^ s)
  | LocalVar i -> lw dest areg (- 4 * (i + 3), fp)
  (* note : first element of the frame are ra, s0, s1, local variables start at $fp - 4 *)
  | ClosureVar i -> lw dest areg (4 * (i + 2), s1)
  (* note : closure variable starts at $s1 + 8 *)
  | ArgVar -> move dest s0

let write_variable src var =
  match var with
  | GlobalVar s -> sw src alab ("G" ^ s)
  | LocalVar i -> sw src areg (- 4 * (i + 3), fp)
  | ClosureVar _ -> (Printf.printf "ICE : trying to write the environment" ; exit 2)
  | ArgVar -> (Printf.printf "ICE : try to write the argument" ; exit 2)

let write_env env (delta, r) =
  List.fold_left
    (fun (code, delta) var ->
     let code =
       code
       ++ read_variable t0 var
       ++ sw t0 areg (delta, r)
     in (code, delta + 4)
    ) (nop, delta) env
  |> fst
(* create the value v on the heap and put its address in v0 *)
(* be sareful it modifies a0 and a lot of others *)

let allocate size =
  li a0 size
  ++ li v0 9
  ++ syscall



module StringMap = Map.Make(String)

let binary_primtives =
  StringMap.empty
  |> StringMap.add "add"  (fun d r1 r2 -> add d r1 oreg r2)
  |> StringMap.add "sub" (fun d r1 r2 -> sub d r1 oreg r2)
  |> StringMap.add "mul"  (fun d r1 r2 -> mul d r1 oreg r2)
  |> StringMap.add "lt"    (slt)
  |> StringMap.add "gt"    (sgt)
  |> StringMap.add "leq"   (sle)
  |> StringMap.add "geq"   (sge)
  |> StringMap.add "eq"    (seq)
  |> StringMap.add "neq"   (sne)
  |> StringMap.add "rem"   (remC)
  |> StringMap.add "div"   (divC)

let compile_alloc a = match a with
  | AImm ->
     allocate 8            ++ comment "enough space for a tag + an integer"
     ++ comment "address of the allocated space is now in $v0"
     ++ li t0 Tag.int      ++ comment "0 is the tag for integers"
  | ACons ->
     allocate 12           ++ comment "space for tag + hd + tl"
     ++ li t0 Tag.cons     ++ comment "tag for cons"
     ++ sw t0 areg (0, v0) ++ comment "store the tag"
  | AClos env_lgth ->
     allocate ((env_lgth + 2) * 4)   ++ comment "space for tag + code ptr + env"
     ++ li t0 Tag.closure            ++ comment "tag for closure"
     ++ sw t0 areg (0, v0)           ++ comment "store tag"
  | AFroz ->
     allocate 8                       ++ comment "space for tag + closure"
     ++ li t0 Tag.frozen              ++ comment "tag for frozen blocks"
     ++ sw t0 areg (0, v0)            ++ comment "store the tag"

let rec compile_store_value v = match v with
  | Imm i ->
     sw t0 areg (0, v0) ++ comment "store the tag"
     ++ li t0 i
     ++ sw t0 areg (4, v0) ++ comment "store the immediate value"
  | Cons ->
     pop t0
     ++ sw t0 areg (8, v0) ++ comment "store the tail"
     ++ pop t0
     ++ sw t0 areg (4, v0) ++ comment "store the head"
  | Clos (l, env) ->
     la t0 alab (compile_label l)    ++ comment "code ptr"
     ++ sw t0 areg (4, v0)           ++ comment "store code ptr"
     ++ write_env env (8, v0)
  | Froz (l, env) ->
     push v0
     ++ compile_alloc (AClos (List.length env))
     ++ compile_store_value (Clos (l, env))
     ++ move t0 v0
     ++ pop v0
     ++ sw t0 areg (4, v0)             ++ comment "store the closure"

let compile_instr ir = match ir with
  | Force ->
     jal "force"
  | Label l ->
     label (compile_label l)
  | Alloc a ->
     compile_alloc a
  | Value v ->
     compile_store_value v (* modify registers (t0, t1, a0, v0) but neither sp nor ra *)
  | Branch l ->
     b (compile_label l)
  | BranchTrue l ->
     lw t0 areg (4, v0)
     ++ bnez t0 (compile_label l)
  | BranchFalse l ->
     lw t0 areg (4, v0)
     ++ beqz t0 (compile_label l)
  | CallFun ->
     move a1 v0
     ++ pop a0
     ++ lw t0 areg (4, a1)
     ++ jalr t0
	    (*   And the result is in v0  *)
  (* TODO : How do we load the environment ? *)
  | StartCall ->
     push fp
     ++ push ra
     ++ move fp sp (* is that right ? *)
     ++ push s0
     ++ push s1
     ++ move s0 a0
     ++ move s1 a1
  | ReturnCall ->
      pop s1
     ++ pop s0
     ++ pop ra
     ++ pop fp
     ++ jr ra
  | ReturnForce -> nop (* No need to do anything *)
  (* To implement once we decided environment representation *)
  | Push -> push v0
  | Pop n -> popn (4 * n)
  | Fetch v -> read_variable v0 v
  | Store v -> write_variable v0 v
  | BinPrim s ->
     pop a0
     ++ lw a0 areg (4, a0)
     ++ lw v0 areg (4, v0)
     ++ (StringMap.find s binary_primtives) t0 a0 v0
     ++ allocate 8
     ++ sw t0 areg (4, v0)
     ++ li t0 Tag.int
     ++ sw t0 areg (0, v0)
  | UnPrim s ->
     begin
       move a0 v0 ++
       match s with
       | "error" -> error
       | "putChar" -> putChar
		      ++ compile_alloc AImm
		      ++ compile_store_value (Imm 0)
       | _ -> Utils.exhaust_pattern ()
     end
  | ApplyCons ->
     comment "Apply cons"
     ++ pop t0
     ++ move t1 v0
     ++ allocate 12
     ++ sw t0 areg (4, v0)
     ++ sw t1 areg (8, v0)
     ++ li t0 Tag.cons
     ++ sw t0 areg (0, v0)
  | ApplyUncons ->
     comment "Apply uncons"
     ++ lw t0 areg (4, v0)
     ++ push t0
     ++ lw t0 areg (8, v0)
     ++ push t0
