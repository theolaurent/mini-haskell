(* Bibliothèque pour produire du code MIPS

   2008 Jean-Christophe Filliâtre (CNRS)
   2013 Kim Nguyen (Université Paris Sud)
*)

open Format

type register =  string
let v0 : register = "$v0"
let v1 : register = "$v1"
let a0 : register = "$a0"
let a1 : register = "$a1"
let a2 : register = "$a2"
let a3 : register = "$a3"
let t0 : register = "$t0"
let t1 : register = "$t1"
let t2 : register = "$t2"
let t3 : register = "$t3"
let s0 : register = "$s0"
let s1 : register = "$s1"
let ra : register = "$ra"
let sp : register = "$sp"
let fp : register = "$fp"
let gp : register = "$gp"
let zero : register = "$zero"

type label = string
type 'a address = formatter -> 'a -> unit
let alab : label address = fun fmt  (s : label) -> fprintf fmt "%s" s
let areg : (int * register) address = fun fmt (x, y) -> fprintf fmt "%i(%s)" x y
type 'a operand = formatter -> 'a -> unit
let oreg : register operand = fun fmt (r : register) -> fprintf fmt "%s" r
let oi : int operand = fun fmt i -> fprintf fmt "%i" i
let oi32 : int32 operand = fun fmt i -> fprintf fmt "%li" i

type 'a asm =
  | Nop
  | S of string
  | Cat of 'a asm * 'a asm

type text = [`text ] asm
type data = [`data ] asm

let buf = Buffer.create 17
let fmt = formatter_of_buffer buf
let ins x =
  Buffer.add_char buf '\t';
  kfprintf (fun fmt ->
    fprintf fmt "\n";
    pp_print_flush fmt ();
    let s = Buffer.contents buf in
    Buffer.clear buf;
    S s
  ) fmt x

let pr_list fmt pr = function
  | []      -> ()
  | [i]     -> pr fmt i
  | i :: ll -> pr fmt i; List.iter (fun i -> fprintf fmt ", %a" pr i) ll

let pr_ilist fmt l =
  pr_list fmt (fun fmt i -> fprintf fmt "%i" i) l

let pr_alist fmt l =
  pr_list fmt (fun fmt (a : label) -> fprintf fmt "%s" a) l

let abs a b = ins "abs %s, %s" a b
let add a b (o : 'a operand) x = ins "add %s, %s, %a" a b o x
let clz a b = ins "clz %s, %s" a b
let and_ a b c = ins "and %s, %s, %s" a b c
let div a b (o : 'a operand) x = ins "div %s, %s, %a" a b o x
let mul a b (o : 'a operand) x = ins "mul %s, %s, %a" a b o x
let or_ a b c = ins "or %s, %s, %s" a b c
let not_ a b = ins "not %s, %s" a b
let rem a b (o : 'a operand) x = ins "rem %s, %s, %a" a b o x
let neg a b = ins "neg %s, %s" a b
let sub a b (o : 'a operand) = ins "sub %s, %s, %a" a b o
let li a b = ins "li %s, %i" a b
let li32 a b = ins "li %s, %li" a b
let seq a b c = ins "seq %s, %s, %s" a b c
let sge a b c = ins "sge %s, %s, %s" a b c
let sgt a b c = ins "sgt %s, %s, %s" a b c
let sle a b c = ins "sle %s, %s, %s" a b c
let slt a b c = ins "slt %s, %s, %s" a b c
let sne a b c = ins "sne %s, %s, %s" a b c
let b (z : label) = ins "b %s" z
let beq x y (z : label) = ins "beq %s, %s, %s" x y z
let bne x y (z : label) = ins "bne %s, %s, %s" x y z
let bge x y (z : label) = ins "bge %s, %s, %s" x y z
let bgt x y (z : label) = ins "bgt %s, %s, %s" x y z
let ble x y (z : label) = ins "ble %s, %s, %s" x y z
let blt x y (z : label) = ins "blt %s, %s, %s" x y z

let beqz x (z : label) = ins "beqz %s, %s" x z
let bnez x (z : label) = ins "bnez %s, %s" x z
let bgez x (z : label) = ins "bgez %s, %s" x z
let bgtz x (z : label) = ins "bgtz %s, %s" x z
let blez x (z : label) = ins "blez %s, %s" x z
let bltz x (z : label) = ins "bltz %s, %s" x z

let jr a = ins "jr %s" a
let jal (z : label) = ins "jal %s" z
let jalr (z : register) = ins "jalr %s" z
let la x (p : 'a address)  = ins "la %s, %a" x p
let lb x (p : 'a address) = ins "lb %s, %a" x p
let lbu x (p : 'a address) = ins "lbu %s, %a" x p
let lw x (p : 'a address) = ins "lw %s, %a" x p
let sb x (p : 'a address) = ins "sb %s, %a" x p
let sw x (p : 'a address) = ins "sw %s, %a" x p
let move a b = ins "move %s, %s" a b
let nop = Nop
let label (s : label) = S (s ^ ":\n")
let syscall = S "\tsyscall\n"
let comment s = S ("#" ^ s ^ "\n")
let align n = ins ".align %i" n
let asciiz s = ins ".asciiz %S" s
let dword l = ins ".word %a" pr_ilist l
let address l = ins ".word %a" pr_alist l
let space n = ins ".space %d" n
let inline s = S s
let (++) x y = Cat (x, y)


let push r =
  sub sp sp oi 4 ++
  sw r areg (0, sp)

let peek r =
  lw r areg (0, sp)

let pop r =
  peek r ++
  add sp sp oi 4

let popn n =
  add sp sp oi n

type program = {
  text : [ `text ] asm;
  data : [ `data ] asm;
}

let rec pr_asm fmt = function
  | Nop          -> ()
  | S s          -> fprintf fmt "%s" s
  | Cat (a1, a2) -> pr_asm fmt a1; pr_asm fmt a2

let print_program fmt p =
  fprintf fmt ".text\n";
  pr_asm fmt p.text;
  fprintf fmt ".data\n";
  pr_asm fmt p.data;
  pp_print_flush fmt ()

let print_in_file ~file p =
  let c = open_out file in
  let fmt = formatter_of_out_channel c in
  print_program fmt p;
  close_out c
