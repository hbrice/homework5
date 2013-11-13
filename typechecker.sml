use "type.sml";
   
(* A modified term datatype to allow us to type our function arguments *)
type identifier = string;
datatype TE =
    Variable of identifier
  | Literal of int 
  | Plus of TE * TE 
  | Lambda of identifier * typ * TE
  | Lett of identifier * TE * TE
  | App of TE * TE 
  | Rec of identifier * typ * TE
  | Bool of bool 
  | IsZero of TE
  | IfThenElse of TE * TE * TE ;

exception Unimplemented
exception TypeError

(* 
 *  I have included the code for AST_NUM and AST_IF; you must complete
 *  the other cases!
 *)

(* typeOf : env -> TE -> typ *)
fun typeOf env (Literal x)       = INT
  | typeOf env (Variable x)      = raise Unimplemented
  | typeOf env (Plus(x, y))      = raise Unimplemented
  | typeOf env (Lambda(i,t,e))   = raise Unimplemented
  | typeOf env (Lett(l,e1,e2))   = raise Unimplemented
  | typeOf env (App(e1,e2))      = raise Unimplemented
  | typeOf env (Rec(i,t,e))      = raise Unimplemented
  | typeOf env (Bool x)          = raise Unimplemented
  | typeOf env (IsZero(x))       = raise Unimplemented
  | typeOf env (IfThenElse(e1,e2,e3)) =
        let val t1 = typeOf env e1
            val t2 = typeOf env e2
            val t3 = typeOf env e3
        in
            if (t1 = BOOL) andalso (t2 = t3)
            then t3
            else raise TypeError
       end;

(*
Some sample functions translated into abstract syntax for you to test
your typechecker on:
*)

(* fn f => fn x => f (f x) *)
val test1 = Lambda("f", ARROW(VAR "a", VAR "a"),
                Lambda("x", VAR "a",
                    App(Variable "f",
                        App(Variable "f", Variable "x"))));

(* fn f => fn g => fn x => f (g x) *)
val test2 = Lambda("f", ARROW(VAR "b", VAR "c"),
                Lambda("g", ARROW(VAR "a", VAR "b"),
                    Lambda("x", VAR "a",
                        App(Variable "f",
                            App(Variable "g", Variable "x")))));

(* fn b => if b then 1 else 0 *)
val test3 = Lambda("b", BOOL,
                IfThenElse(Variable "b", Literal 1, Literal 0));


(* feel free to write your own test expressions! *)