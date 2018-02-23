(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
open List
    
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
      | _ -> failwith "Wrong expression"
                                                         
  end
