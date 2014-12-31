
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

(* current environement is pointed by register ... *)

let compile_label (L l) = ("L" ^ string_of_int l)

(* force applies forcing algorithm on value pointed by v0 *)			    
let force =
  label "force"
  ++ lw a0 areg (0, v0)
  ++ li a1 Tag.frozen    ++ comment "frozen tag"
  ++ bge a0 a1 "force_frz"
  ++ jr ra
  ++ label "force_frz"
  ++ beq a0 a1 "force_frozen"
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
  ++ pop a0             ++ comment "restore forced value in a0"
  ++ li a1 Tag.unfrozen ++ comment "unfrozen tag"
  ++ sw a1 areg (0, a0) ++ comment "update tag"
  ++ sw v0 areg (4, a0) ++ comment "update the value"
  ++ pop ra
  ++ jr ra
	 		    
(* Store the env in memory [ *r + delta ; *r + delta + 4 * (length env - 1)] 
   r must be different from a1, sp, ra
   I've assumed the environment starts at sp but I'm probably wrong...  
 *)			    
let write_env env (delta, r) =
  List.fold_left
    (fun (code, delta) i ->
     let code =
       code 
       ++ add a1 sp oi (-4 * i)
       ++ sw a1 areg (delta, r)
     in (code, delta + 4)
    ) (nop, delta) env
  |> fst  
(* create the value v on the heap and put its address in v0 *)
(* be sareful it modifies a0 and a lot of others *)
let rec compile_value v = match v with
  | Imm i ->
     li a0 8            ++ comment "enough space for a tag + an integer"
  ++ li v0 9            ++ comment "syscall 9 (sbrk)"
  ++ syscall
  ++ comment "address of the allocated space is now in $v0"
  ++ li a1 Tag.int      ++ comment "0 is the tag for integers"
  ++ sw a1 areg (0, v0) ++ comment "store the tag"
  ++ li a2 i
  ++ sw a2 areg (4, v0) ++ comment "store the immediate value"
  | Cons (v1, v2) ->
     li a0 12              ++ comment "space for tag + hd + tl"
     ++ li v0 9            ++ comment "sbrk"
     ++ syscall
     ++ li a1 Tag.cons     ++ comment "tag for cons"
     ++ sw a1 areg (0, v0) ++ comment "store the tag"
     ++ push v0            ++ comment "save addresse of allocated space"
     ++ comment "allocate head"
     ++ compile_value v1
     ++ peek a0
     ++ sw v0 areg (4, a0) ++ comment "store the head"
     ++ comment "allocate tail"
     ++ compile_value v2  
     ++ pop a0
     ++ sw v0 areg (8, a0) ++ comment "store the tail"
     ++ move v0 a0
  | Clos (l, env) ->
     let env_lgth = List.length env in
     li a0 ((env_lgth + 2) * 4)      ++ comment "space for tag + code ptr + env"
     ++ li v0 9                      ++ comment "sbrk"
     ++ syscall
     ++ li a1 Tag.closure            ++ comment "tag for closure"
     ++ sw a1 areg (0, v0)           ++ comment "store tag"
     ++ lw a1 alab (compile_label l) ++ comment "code ptr"
     ++ sw a1 areg (4, v0)           ++ comment "store code ptr"
     ++ write_env env (8, v0)
  | Froz (l, env) ->
     li a0 8                          ++ comment "space for tag + closure"
     ++ li v0 9
     ++ syscall
     ++ li a1 Tag.frozen              ++ comment "tag for frozen blocks"
     ++ sw a1 areg (0, v0)            ++ comment "store the tag"
     ++ push v0
     ++ compile_value (Clos (l, env))
     ++ move a1 v0
     ++ pop v0
     ++ lw a1 areg (4, v0)             ++ comment "store the closure"
	   

let compile_instr ir = match ir with
  | Force ->     
     jal "force"
  | Label l ->
     label (compile_label l)
  | Value v ->
     compile_value v (* modify registers but neither sp nor ra *)
  | Branch l ->
     b (compile_label l)
  | BranchFalse l ->
     lw v0 areg (4, v0) 
     ++ beqz v0 (compile_label l)
  | CallFun -> failwith "TODO"
   (* Potentially :
     move a1 v0
     ++ pop a0
     ++ lw t0 (4, a1)
     ++ push sp
     ++ push ra
     ++ jalr t0
     ++ pop ra
     ++ pop sp
   And the result is in v0 
   TODO : How do we load the environment ? *) 
  | ReturnCall -> jr ra
  | ReturnForce -> failwith "TODO" (* Not needed *)
  (* To implement once we decided environment representation *)
  | Fetch i -> failwith "TODO"
  | Store i -> failwith "TODO"
