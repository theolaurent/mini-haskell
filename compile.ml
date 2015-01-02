
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

(* a0 and a1 should always be restored (they contains the current argument and closure respectively *)

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
  ++ lw t1 areg (4, v0) ++ comment "load the closure"
  ++ lw t0 areg (4, t1) ++ comment "load the code ptr"
  ++ jalr t0            ++ comment "evaluate function"
  ++ jal "force"        ++ comment "recursively apply force on the result"
  ++ comment "At this point, the result of this force is in v0"
  ++ pop t0             ++ comment "restore forced value in t0"
  ++ li t1 Tag.unfrozen ++ comment "unfrozen tag"
  ++ sw t1 areg (0, t0) ++ comment "update tag"
  ++ sw v0 areg (4, t0) ++ comment "update the value"
  ++ pop ra
  ++ jr ra


let error =
  label "error"
  ++ move t0 a0
  ++ la a0 alab "error_msg"
  ++ li v0 4
  ++ syscall            ++ comment "print string"
  ++ move a0 t0
  ++ lw t0 areg (0, a0) ++ comment "load the tag of the list"
  ++ li t1 Tag.cons     ++ comment "tag of cons"
  ++ li v0 11
  ++ j "end_loop_error"
  ++ label "start_loop_error"
  ++ lw t0 areg (8, a0) ++ comment "load the tail"
  ++ lw a0 areg (4, a0) ++ comment "load the head"
  ++ syscall            ++ comment "print char"
  ++ move a0 t0
  ++ lw t0 areg (0, a0)
  ++ label "end_loop_error"
  ++ beq t0 t1 "start_loop_error"
  ++ li a0 2
  ++ li v0 10
  ++ syscall

       
	
let error_msg =
  label "error_msg"
  ++ asciiz "error : "
	
(* Store the env in memory [ *r + delta ; *r + delta + 4 * (length env - 1)]
   r must be different from a1, sp, ra
   I've assumed the environment starts at sp but I'm probably wrong...
 *)
let read_variable dest var =
  match var with
  | GlobalVar s -> lw dest alab s
  | LocalVar i -> lw dest areg (- 4 * (i + 3), fp)
  (* note : first element of the frame are ra, s0, s1, local variables start at $fp - 4 *)
  | ClosureVar i -> lw dest areg (4 * (i + 2), s1)
  (* note : closure variable starts at $s1 + 8 *)
  | ArgVar -> move dest s0

let write_variable src var =
  match var with
  | GlobalVar s -> sw src alab s
  | LocalVar i -> sw src areg (- 4 * (i + 3), fp)
  | ClosureVar i -> (Printf.printf "ICE : trying to write the environment" ; exit 2)
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
  |> StringMap.add "plus"  (fun d r1 r2 -> add d r1 oreg r2)
  |> StringMap.add "minus" (fun d r1 r2 -> sub d r1 oreg r2)
  |> StringMap.add "mult"  (fun d r1 r2 -> mul d r1 oreg r2)
  |> StringMap.add "lt"    (slt)
  |> StringMap.add "gt"    (sgt)
  |> StringMap.add "leq"   (sle)
  |> StringMap.add "geq"   (sge)
  |> StringMap.add "eq"    (seq)
  |> StringMap.add "neq"   (sne)


       
let rec compile_value v = match v with
  | Imm i ->
     allocate 8            ++ comment "enough space for a tag + an integer"
     ++ comment "address of the allocated space is now in $v0"
     ++ li t0 Tag.int      ++ comment "0 is the tag for integers"
     ++ sw t0 areg (0, v0) ++ comment "store the tag"
     ++ li t0 i
  ++ sw t0 areg (4, v0) ++ comment "store the immediate value"
  | Cons (v1, v2) ->
     allocate 12           ++ comment "space for tag + hd + tl"
     ++ li t0 Tag.cons     ++ comment "tag for cons"
     ++ sw t0 areg (0, v0) ++ comment "store the tag"
     ++ push v0            ++ comment "save addresse of allocated space"
     ++ comment "allocate head"
     ++ compile_value v1
     ++ peek t0
     ++ sw v0 areg (4, t0) ++ comment "store the head"
     ++ comment "allocate tail"
     ++ compile_value v2
     ++ pop t0
     ++ sw v0 areg (8, t0) ++ comment "store the tail"
     ++ move v0 t0
  | Clos (l, env) ->
     let env_lgth = List.length env in
     allocate ((env_lgth + 2) * 4)   ++ comment "space for tag + code ptr + env"
     ++ li t0 Tag.closure            ++ comment "tag for closure"
     ++ sw t0 areg (0, v0)           ++ comment "store tag"
     ++ la t0 alab (compile_label l) ++ comment "code ptr"
     ++ sw t0 areg (4, v0)           ++ comment "store code ptr"
     ++ write_env env (8, v0)
  | Froz (l, env) ->
     allocate 8                       ++ comment "space for tag + closure"
     ++ li t0 Tag.frozen              ++ comment "tag for frozen blocks"
     ++ sw t0 areg (0, v0)            ++ comment "store the tag"
     ++ push v0
     ++ compile_value (Clos (l, env))
     ++ move t0 v0
     ++ pop v0
     ++ lw t0 areg (4, v0)             ++ comment "store the closure"
						  
and compile_instr ir = match ir with
  | Force ->
     jal "force"
  | Label l ->
     label (compile_label l)
  | Value v ->
     compile_value v (* modify registers (t0, t1, a0, v0) but neither sp nor ra *)
  | Branch l ->
     b (compile_label l)
  | BranchTrue l ->
     lw t0 areg (4, v0)
     ++ bnez t0 (compile_label l)
  | BranchFalse l ->
     lw t0 areg (4, v0)
     ++ beqz t0 (compile_label l)
  | CallFun ->
     move t0 a0
     ++ move t1 a1
     ++ move a1 v0
     ++ pop a0
     ++ push t0
     ++ push t1
     ++ lw t0 areg (4, a1)
     ++ push fp
     ++ push ra
     ++ jalr t0
     ++ pop ra
     ++ pop fp
     ++ pop a1
     ++ pop a0
	    (*   And the result is in v0  *)
   (* TODO : How do we load the environment ? *)
  | ReturnCall ->
      pop s1
     ++ pop s0
     ++ pop ra
     ++ 
     jr ra
  | ReturnForce -> nop (* No need to do anything *)
  (* To implement once we decided environment representation *)		     
  | Alloc n -> sub sp sp oi (4 * n)
  | DeAlloc n -> popn (4 * n)
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
  | ApplyCons ->
     pop t0
     ++ move t1 v0
     ++ allocate 12
     ++ sw t0 areg (4, v0)
     ++ sw t0 areg (8, v0)
     ++ li t0 Tag.cons
     ++ sw t0 areg (0, v0)
