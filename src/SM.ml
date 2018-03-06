open GT
open List
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Language.Stmt.config

(*
	Evaluates single operation of stack machine
	val evalSingleOperation : config -> insn -> config
*)
let evalSingleOperation (stack, (state, input, output)) operation = match operation with
	| BINOP op -> 
		let hd1 :: hd2 :: tl = stack in
		let const1 = Expr.Const hd1 in
		let const2 = Expr.Const hd2 in
		let res = Expr.eval state (Expr.Binop (op, const1, const2)) in
		(res :: tl, (state, input, output))
	| CONST x -> (x :: stack, (state, input, output))
	| READ -> ((hd input) :: stack, (state, tl input, output))
	| WRITE -> (tl input, (state, input, (hd stack) :: output))
	| LD s -> ((state s) :: stack, (state, input, output))
	| ST s -> (tl stack, (Expr.update s (hd stack) state, input, output))

(* Stack machine interpreter
     val eval : config -> prg -> config
   Takes a configuration and a program, and returns a configuration as a result
 *)     
let rec eval fullConfig program = match program with
	| [] -> fullConfig
	| hd :: tl -> eval (evalSingleOperation fullConfig hd) tl

let rec compileExpression expr = match expr with
	| Expr.Const n -> [CONST n]
	| Expr.Var s -> [LD s]
	| Expr.Binop (operation, x, y) -> (compileExpression x) @ (compileExpression y) @ [BINOP operation]


(* Top-level evaluation
     val run : prg -> int list -> int list
   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler
     val compile : Syntax.Stmt.t -> prg
   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec compile statement = match statement with
	| Stmt.Read s -> [READ ; ST s]
	| Stmt.Write expr -> (compileExpression expr) @ [WRITE]
	| Stmt.Assign (s, expr) -> (compileExpression expr) @ [ST s]
	| Stmt.Seq (st1, st2) -> (compile st1) @ (compile st2)
