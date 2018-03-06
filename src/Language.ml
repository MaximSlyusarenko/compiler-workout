(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT
open List

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
open Ostap
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    let intToBool x = x != 0

    let boolToInt x = if x then 1 else 0

    (* Expression evaluator
       val eval : state -> expr -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    let rec eval state expression = match expression with
      | Const x -> x
      | Var x -> state x
      | Binop ("!!", x, y) -> evalBooleanFunctionWithBooleanArgs (||) state x y
      | Binop ("&&", x, y) -> evalBooleanFunctionWithBooleanArgs (&&) state x y
      | Binop ("==", x, y) -> evalBooleanFunctionWithIntArgs (==) state x y
      | Binop ("!=", x, y) -> evalBooleanFunctionWithIntArgs (!=) state x y
      | Binop ("<=", x, y) -> evalBooleanFunctionWithIntArgs (<=) state x y
      | Binop ("<", x, y) -> evalBooleanFunctionWithIntArgs (<) state x y
      | Binop (">=", x, y) -> evalBooleanFunctionWithIntArgs (>=) state x y
      | Binop (">", x, y) -> evalBooleanFunctionWithIntArgs (>) state x y
      | Binop ("+", x, y) -> evalIntFunctionWithIntArgs (+) state x y
      | Binop ("-", x, y) -> evalIntFunctionWithIntArgs (-) state x y
      | Binop ("*", x, y) -> evalIntFunctionWithIntArgs ( * ) state x y
      | Binop ("/", x, y) -> evalIntFunctionWithIntArgs (/) state x y
      | Binop ("%", x, y) -> evalIntFunctionWithIntArgs (mod) state x y
      | _ -> failwith "Wrong expression"

    and evalBooleanFunctionWithBooleanArgs func state x y = boolToInt (func (intToBool (eval state x)) (intToBool (eval state y)))
    and evalBooleanFunctionWithIntArgs func state x y = boolToInt (func (eval state x) (eval state y))
    and evalIntFunctionWithIntArgs func state x y = func (eval state x) (eval state y)

    let constructOpsList lst = List.map (fun op -> (ostap ($(op)), fun x y -> Binop (op, x, y))) lst

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
   
    *)
    ostap (
      primary: s:IDENT {Var s} | x:DECIMAL {Const x} | -"(" parse -")";

      parse: !(Util.expr
        (fun x -> x)
        [|
          `Lefta, constructOpsList ["!!"];
          `Lefta, constructOpsList ["&&"];
          `Nona,  constructOpsList ["<="; ">="; "<"; ">"; "=="; "!="];
          `Lefta, constructOpsList ["+"; "-"];
          `Lefta, constructOpsList["*"; "/"; "%"]
        |]
        primary
      )
    )

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval (state, input, output) expression = match expression with
      | Read s -> (Expr.update s (hd input) state, tl input, output)
      | Write expr -> (state, input, (Expr.eval state expr) :: output)
      | Assign (s, expr) -> (Expr.update s (Expr.eval state expr) state, input, output)
      | Seq (st1, st2) -> eval (eval (state, input, output) st1) st2

    (* Statement parser *)
    ostap (
      primary: -"read" -"(" z:IDENT -")" {Read z} | -"write" -"(" e:!(Expr.parse) -")" {Write e} | left:IDENT -":=" right:!(Expr.parse) {Assign (left, right)};

      parse: !(Ostap.Util.expr
        (fun x -> x)
        [|
          `Righta, [ostap (";"), fun x y -> Seq(x, y)]
        |]
        primary
      )
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
