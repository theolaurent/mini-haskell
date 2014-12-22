
(* Bibliothèque pour produire du code MIPS

   2008 Jean-Christophe Filliâtre (CNRS)
   - version initiale

   2013 Kim Nguyen (Université Paris Sud)
   - sous-types text et data
   - types fantômes pour oreg et oi
   - plus d'opérations et de directives
   - manipulation de la pile
   - ocamldoc
*)

(** {0 Bibliothèque pour l'écriture de programmes MIPS } *)


(** Le module {!Mips} permet l'écriture de code MIPS dans du code
    OCaml, sans utiliser un préprocesseur.  Un exemple complet est
    donné {{:#1_Exemple}ci-dessous, dans la section exemple}. *)

type 'a asm
(** type abstrait pour représenter du code assembleur. Le paramètre
    ['a] est utilisé comme type fantôme. *)

type text = [ `text ] asm
(** type représentant du code assembleur se trouvant dans la zone de
    texte *)

type data = [ `data ] asm
(** type représentant du code assembleur se trouvant dans la zone de
    données *)

type program = {
  text : text;
  data : data;
}
(** un programme est constitué d'une zone de texte et d'une zone de
    donnée *)

val print_program : Format.formatter -> program -> unit
  (** [print_program fmt p] imprime le code du programme [p] dans le
      formatter [fmt] *)

val print_in_file: file:string -> program -> unit

type register
(** Type abstrait pour les registres *)

val v0 : register
val v1 : register
val a0 : register
val a1 : register
val a2 : register
val a3 : register
val t0 : register
val t1 : register
val t2 : register
val t3 : register
val s0 : register
val s1 : register
val ra : register
val sp : register
val fp : register
val gp : register
val zero : register
(** Constantes représentant les registres manipulables. [zero] est
    cablé à 0 *)


type label = string
(** Les étiquettes d'addresses sont des chaines de caractères *)

type 'a operand
val oreg : register operand
val oi : int operand
val oi32 : int32 operand

(** type abstrait pour représenter la dernière opérande d'une
    expression arithmétique ainsi que 3 constantes (soit un registre,
    soit un entier, soit un entier 32 bits)
*)



(** {1 Opérations arithmétiques } *)


val li : register -> int -> text
val li32 : register -> int32 -> text
(** Chargement des constantes entières *)

val abs : register -> register -> text
(** [abs r1 r2] stocke dans r1 la valeur absolue de r2 *)

val neg : register -> register -> text
(** [neg r1 r2] stocke dans r1 l'opposé de r2 *)

val add : register -> register -> 'a operand -> 'a -> text
val sub : register -> register -> 'a operand -> 'a -> text
val mul : register -> register -> 'a operand -> 'a -> text
val rem : register -> register -> 'a operand -> 'a -> text
val div : register -> register -> 'a operand -> 'a -> text

(** Les 5 opérations arithmétique de base: [add rdst rsrc1 ospec o]
   stocke dans rdst le résultat de l'opération entre rsrc1 et o. La
   constant ospec spécifie si o est un immédiat, immédiat sur 32 bits
   ou un registre.
   Exemple:

   [add v0 v1 oreg v2]

   [div v0 v1 oi 424]

   [sub t0 a0 oi32 2147483647l]
 *)

(** {1 Opérations logiques } *)

val and_ : register -> register -> register -> text
val or_ : register -> register -> register -> text
val not_ : register -> register -> text
val clz : register -> register -> text
(** Opérations de manipulation de bits. "et" bit à bit, "ou" bit à
    bit, "not" bit à bit et clz (count leading zero) *)


(** {1 Comparaisons } *)

val seq : register -> register -> register -> text
val sge : register -> register -> register -> text
val sgt : register -> register -> register -> text
val sle : register -> register -> register -> text
val slt : register -> register -> register -> text
val sne : register -> register -> register -> text
  (** conditionnelles [sop ra rb rc] met [ra] à 1 si [rb op rc] et à 0
      dans le cas contraire (eq : ==, ge : >=, gt : >, le : <=, lt : <=,
      ne : !=) *)

(** {1 Sauts } *)

val b : label -> text
(** saut inconditionnel *)

val beq : register -> register -> label -> text
val bne : register -> register -> label -> text
val bge : register -> register -> label -> text
val bgt : register -> register -> label -> text
val ble : register -> register -> label -> text
val blt : register -> register -> label -> text
(** [bop ra rb label] branche vers le label [label] si [ra op rb] *)

val beqz : register -> label -> text
val bnez : register -> label -> text
val bgez : register -> label -> text
val bgtz : register -> label -> text
val blez : register ->  label -> text
val bltz : register ->  label -> text
(** [bopz ra rb label] branche vers le label [label] si [ra op 0] *)


val jr : register -> text
(** [jr r] Continue l'exécution à l'adresse spécifiée dans le registre
    [r] *)
val jal : label -> text
(** [jal l] Continue l'exécution à l'adresse spécifiée par le label [l],
    sauve l'adresse de retour dans $ra.
*)
val jalr : register -> text
(** [jalr r] Continue l'exécution à l'adresse spécifiée par le
    registre [r], sauve l'adresse de retour dans $ra.
*)

(** {1 Lecture / écriture en mémoire } *)

type 'a address
(** type abstrait pour représenter des adresses *)

val alab : label address
val areg : (int * register) address
(** Les adresses sont soit données par un label, soit par une paire
    décalage, registre *)

val la : register -> 'a address -> 'a -> text
(** [la reg alab "foo"] charge dans [reg] l'adresse du label "foo"
    [la reg1 areg (x, reg2)] charge dans [reg1] l'adresse contenue dans
    [reg2] décallée de [x] octets
 *)

val lbu : register -> 'a address -> 'a -> text
(** charge l'octet à l'adresse donnée sans extension de signe (valeur
    entre 0 et 255) *)
val lw : register -> 'a address -> 'a -> text
(** charge l'entier 32bits à l'adresse donnée *)
val sb : register -> 'a address -> 'a -> text
(** écrit les 8 bits de poid faible du registre donnée à l'adresse
    donnée *)
val sw : register -> 'a address -> 'a -> text
(** écrit le contenu du registre à l'adresse donnée *)
val move : register -> register -> text

(** {1 Divers } *)

val nop : [> ] asm
(** l'instruction vide. Peut se trouver dans du text ou du data *)

val label : label ->  [> ] asm
(** un label. Peut se retrouver dans du text ou du data *)

val syscall : text
(** l'instruction syscall *)

val comment : string -> [> ] asm
(** place un commentaire dans le code généré. Peut se retrouver dans
    du text ou du data *)

val align : int ->  [> ] asm
(** [align n] aligne le code suivant l'instruction sur 2^n octets *)

val asciiz : string -> data
(** place une constante chaîne de carctères (terminées par 0) dans a
    zone data *)

val dword : int list -> data
(** place une liste de mots mémoires dans la zone data *)

val address : label list -> data
(** place une liste d'adresses (dénotées par des labels) dans la zone
    data *)

val space: int -> data
(** [space n] alloue [n] octets dans le segment de données *)

val inline: string -> [> ] asm
(** [inline s] recopie la chaîne [s] telle quelle dans le fichier assembleur *)

val ( ++ ) : ([< `text|`data ] asm as 'a)-> 'a -> 'a
(** concatène deux bouts de codes (soit text avec text, soit data avec
    data) *)

(** {1 Manipulation de la pile} *)

val push : register -> text
(** [push r] place le contenu de [r] au sommet de la pile.
    Rappel : $sp pointe sur l'adresse de la dernière case occupée *)

val pop : register -> text
(** [pop r] place le mot en sommet de pile dans [r] et dépile *)

val popn: int -> text
(** [popn n] dépile [n] octets *)

val peek : register -> text
(** [peek r] place le mot en sommet de pile dans [r] sans dépiler *)

(** {1 Exemple } *)

(** Le programme ci-dessous, donné à gauche en pur MIPS et à droite en
    OCaml, charge deux constantes, effectue quelques opérations
    arithétiques et affiche le résultat à l'écran

    {[
        .text                                                |  { text =
        main:                                                |        label "main"
         #charge 42 dans $a0 et 23 dans $a1                  |    ++  comment "charge 42 dans $a0 et 23 dans $a1"
         li $a0,  42                                         |    ++  li  a0 42
         li $a1,  23                                         |    ++  li  a1 23
         mul $a0, $a0, $a1                                   |    ++  mul a0 a0 oreg a1 (* on utilise oreg pour dire que la dernière
                                                             |                             operande est un registre *)
         #place le contenu de $a0 sur la pile                |    ++  comment "place le contenu de $a0 sur la pile"
         sub $sp, $sp, 4                                     |    ++  sub sp sp oi 4
         sw  $a0,  0($sp)                                    |    ++  sw a0 areg (0, sp)
                                                             |
         #appelle une routine d'affichage                    |    ++  comment "appelle la routine d'affichage"
         jal print_int                                       |    ++  jal "print_int"
                                                             |
         #termine                                            |    ++  comment "termine"
         li $v0, 10                                          |    ++  li v0 10
         syscall                                             |    ++  syscall
                                                             |
      print_int:                                             |    ++  label "print_int"
         lw $a0,  0($sp)                                     |    ++  lw a0 areg (0, sp)
         add $sp, $sp, 4                                     |    ++  add sp sp oi 4
         li $v0, 1                                           |    ++  li v0 1
         syscall                                             |    ++  syscall
         #affiche un retour chariot                          |    ++  comment "affiche un retour chariot"
         la $a0, newline                                     |    ++  la a0 alab "newline"
         li $v0, 4                                           |    ++  li v0  4
         syscall                                             |    ++  syscall
         jr $ra                                              |    ++  jr ra  ; (* fin du label text *)
                                                             |
        .data                                                |    data =
       newline:                                              |        label "newline"
        .asciiz  "\n"                                        |    ++  asciiz "\n" ;
                                                             |  } (* fin du record *)
    ]}
*)
