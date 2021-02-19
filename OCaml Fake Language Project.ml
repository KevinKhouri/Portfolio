(* Q0  : Get familiar with the external syntax of MiniML *)
let parse_tests : (string * (string, exp) either) list = [
    (* Provide your tests for the parser *)
  ("1;", Right (Int 1));
  (valid_program_1, Right
     (Let
        ([Val
            (Rec ("apply", TArrow (TArrow (TInt, TInt), TArrow (TInt, TInt)),
                  Fn
                    ("f", Some (TArrow (TInt, TInt)),
                     Fn ("x", Some TInt, Apply (Var "f", Var "x")))),
             "apply")],
         Apply
           (Apply (Var "apply", Fn ("x", None, Primop (Times, [Var "x"; Int 3]))),
            Int 100))))
]

(*I comment out my test cases because I checked they work, so I want to reduce
the warnings i get form non-exhaustive pattern matching in the test cases which slows down
eval speed.*)

let free_vars_tests : (exp * name list) list = [
  (* (Int 10, []);
  (let Right(e) = (P.parse "let val x = 3 in 4 * y end;") in e, ["y"]);
  (let Right(e) = (P.parse "let val x = x in let val y = 4 in x+5 end end;") in e, ["x"]);
  (let Right(e) = (P.parse "let val x = 3 in let val x = 4 + y in x + x end end;") in e, ["y"]);
  (let Right(e) = (P.parse "let fun test (x : int) : int = y in 4 end;") in e, ["y"]);
  (let Right(e) = (P.parse "let val x = 3 val (y1,y2) = (x, z) in x + y1 end;") in e, ["z"]);
  (let Right(e) = (P.parse "let val y = (let val x = 2 - z in 3 end) val x = 4 in x + y end;") in e, ["z"]);
  (let Right(e) = (P.parse "let val x = 3 in let val x = x in 2 + 4 end end;") in e, []);
  (Let ([Val (Rec ("x", TArrow (TInt, TInt), Fn ("x", Some TInt, Var "x")), "x")], Int 4),
   [])
  *)
]

(* Q1  : Find the free variables in an expression *)
let rec free_vars (e : exp) : name list = match e with
  | Int n -> []
  | Bool b -> []
  | If(e,e1,e2) -> 
      union (free_vars e) (union (free_vars e1) (free_vars e2))
  | Primop (po, args) -> 
      List.fold_right (fun e1 fv -> union (free_vars e1) (fv)) args []
  | Tuple expL -> 
      List.fold_right (fun e1 fv -> union (free_vars e1) (fv)) expL []
  | Fn (n,t,e) -> 
      delete ([n]) (free_vars(e))
  | Rec (n,t,e) -> 
      delete ([n]) (free_vars(e))
  | Let (decL, e) -> 
      let (freeV, names) = List.fold_left (fun (fv, nms) dec -> match dec with
          | Val(e1,n) -> (union (fv) (delete nms (free_vars (e1))), union [n] nms)
          | Valtuple (e1,nL) -> (union (fv) (delete nms (free_vars (e1))), union nL nms)
          | ByName (e1,n) ->  (union (fv) (delete nms (free_vars (e1))), union [n] nms)
        ) ([],[]) decL 
      in
      union (freeV) (delete (names) (free_vars (e)))
  | Apply (e,e1) -> 
      union (free_vars (e)) (free_vars (e1))
  | Var (n) -> [n]
  | Anno (e,t) -> free_vars e 
;;


let unused_vars_tests : (exp * name list) list = [
  (*(match P.parse valid_program_1 with
      |Right(e) -> (e,[])
      |Left(e) -> type_fail "Invalid Expression");
   (match P.parse valid_program_2 with
    |Right(e) -> (e,[])
    |Left(e) -> type_fail "Invalid Expression");
   (match P.parse valid_program_3 with
    |Right(e) -> (e,[])
    |Left(e) -> type_fail "Invalid Expression");
   (match P.parse valid_program_4 with
    |Right(e) -> (e,[])
    |Left(e) -> type_fail "Invalid Expression");
   (match P.parse valid_program_5 with
    |Right(e) -> (e,[])
    |Left(e) -> type_fail "Invalid Expression");
   (match P.parse valid_program_6 with
    |Right(e) -> (e,["x"])
    |Left(e) -> type_fail "Invalid Expression");
   (match P.parse valid_program_7 with
    |Right(e) -> (e,[])
    |Left(e) -> type_fail "Invalid Expression");
   (match P.parse valid_program_8 with
    |Right(e) -> (e,[])
    |Left(e) -> type_fail "Invalid Expression");
   (match P.parse valid_program_9 with
    |Right(e) -> (e,[])
    |Left(e) -> type_fail "Invalid Expression");
   (match P.parse valid_program_10 with
    |Right(e) -> (e,["y"])
    |Left(e) -> type_fail "Invalid Expression");
   (let Right(e) = (P.parse "let val x = 3 in 4 end;") in e, ["x"]);
   (let Right(e) = (P.parse "let val x = true in let val y = 4 in x+5 end end;") in e, ["y"]);
   (let Right(e) = (P.parse "let val x = 3 in let val x = 4 in x + x end end;") in e, ["x"]);
   (let Right(e) = (P.parse "let fun test (x : int) : int = 3 in 4 end;") in e, ["test";"x"]);
   (let Right(e) = (P.parse "let val x = 3 val (y1,y2) = (x, 2) in x + y1 end;") in e, ["y2"]);
   (let Right(e) = (P.parse "let val y = (let val x = 2 in 3 end) val x = 4 name y = 2 in x + y end;") in e, ["y";"x"]);
   (let Right(e) = (P.parse "let val x = 3 in let val x = fn x => x in 2 + 4 end end;") in e, ["x"]);
  *)
]

(* Q2  : Check variables are in use *)
let rec unused_vars (e : exp) : name list = match e with 
  | Int i -> 
      []
  | Bool b -> 
      []
  | If (e,e1,e2) -> 
      union (unused_vars e) (union (unused_vars e1) (unused_vars e2))
  | Primop (p,expL) ->
      List.fold_right (fun e1 uv -> union (unused_vars e1) (uv)) expL []
  | Tuple (expL) -> 
      List.fold_right (fun e1 uv -> union (unused_vars e1) (uv)) expL []
  | Fn (n,t,e) -> 
      if member n (free_vars e) 
      then unused_vars (e) 
      else union [n] (unused_vars (e))
  | Rec (n,t,e) ->
      (unused_vars(e)) (*according to a TA, a recursive function name should never be considered as unused,
                       even if its not used recursively.*)
  | Let (decL,e) -> (match decL with
      | [] -> unused_vars e
      | (Val (e1, nm))::decs -> 
          if List.mem nm (free_vars (Let(decs, e))) 
          then union (unused_vars e1) (unused_vars (Let(decs, e)))
          else union [nm] (union (unused_vars e1) (unused_vars (Let(decs, e)))) 
      | (Valtuple (e1, nmL))::decs -> 
          let unusedInTuple = List.fold_left (fun unusL var ->
              if List.mem var (free_vars (Let(decs, e))) then unusL
              else union [var] unusL) ([]) nmL in
          union unusedInTuple (union (unused_vars e1) (unused_vars (Let(decs, e))))
      | (ByName (e1, nm))::decs ->   
          if List.mem nm (free_vars (Let(decs, e))) 
          then union (unused_vars e1) (unused_vars (Let(decs, e)))
          else union [nm] (union (unused_vars e1) (unused_vars (Let(decs, e)))) 
    )
  | Apply (e1,e2) -> union (unused_vars e1) (unused_vars e2)
  | Var n -> [] 
  | Anno (e,t) -> unused_vars e
                   
      


let subst_tests : (((exp * name) * exp) * exp) list = [
  (*(match P.parse valid_program_1 with (*Commented out tests because they aren't properly implemented.*)
      |Right(e) -> (((Int 5, "x"),e), 
                    Let
                      ([Val
                          (Rec ("apply", TArrow (TArrow (TInt, TInt), TArrow (TInt, TInt)),
                                Fn
                                  ("f", Some (TArrow (TInt, TInt)),
                                   Fn ("x", Some TInt, Apply (Var "f", Var "x")))),
                           "apply")],
                       Apply
                         (Apply (Var "apply", Fn ("x", None, Primop (Times, [Var "x"; Int 3]))),
                          Int 100)))
      |Left(e) -> type_fail "Invalid Expression"); 
   (match P.parse valid_program_3 with
    |Right(e) -> (((Int 5, "x"),e),e)
    |Left(e) -> type_fail "Invalid Expression"); 
   (match P.parse valid_program_5 with
    |Right(e) -> (((Int 5, "x"),e),e)
    |Left(e) -> type_fail "Invalid Expression");
   (match P.parse valid_program_6 with
    |Right(e) -> (((Int 5, "x"),e),e)
    |Left(e) -> type_fail "Invalid Expression");
   (match P.parse valid_program_7 with
    |Right(e) -> (((Int 5, "x"),e),e)
    |Left(e) -> type_fail "Invalid Expression");
   (match P.parse valid_program_8 with
    |Right(e) -> (((Int 5, "x"),e),e)
    |Left(e) -> type_fail "Invalid Expression");
   (match P.parse valid_program_9 with
    |Right(e) -> (((Int 5, "x"),e),e)
    |Left(e) -> type_fail "Invalid Expression");
   (match P.parse valid_program_10 with
    |Right(e) -> (((Int 5, "x"),e),e)
    |Left(e) -> type_fail "Invalid Expression");
   (((let Right(e) = P.parse "10 * 10 + 33;" in e, "y"), (*Is it correct to have brackets in the test???*) 
     let Right(e) = (P.parse "let val x = 3 in 4 * y end;") in e), 
    let Right(e) = P.parse "let val x = 3 in 4 * (10 * 10 + 33) end;" in e);
   (((let Right(e) = P.parse valid_program_1 in e, "x"),
     let Right(e) = P.parse "let val x = x in let val y = 4 in x + 5 end end;" in e), 
    let Right(e) = 
      P.parse ("let val x = (let fun apply (f : int -> int) : int -> int = fn x : int => f(x) in apply (fn x => x * 3) 100 end) in let val y = 4 in x + 5 end end;") in e);
   (((let Right(e) = P.parse valid_program_5 in e, "y"),
     let Right(e) = P.parse "let val x = 3 in let val x = y in x + x end end;" in e),
    let Right(e) = P.parse ("let val x = 3 in let val x = (let val x = 1 in x + 5 end) in x + x end end;") in e);
   (((let Right(e) = P.parse "5;" in e, "y"),
     (let Right(e) = P.parse "let fun test (x : int) : int = y in 4 end;" in e)),
    let Right(e) = P.parse "let fun test (x : int) : int = 5 in 4 end;" in e);
   (((let Right(e) = P.parse "(3,4);" in e, "z"),
     (let Right(e) = P.parse "let val x = 3 val (y1,y2) = z in x + y1 end;" in e)), 
    let Right(e) = P.parse "let val x = 3 val (y1,y2) = (3,4) in x + y1 end;" in e);
   (((let Right(e) = P.parse "fn x => x;" in e, "z"),
     (let Right(e) = P.parse "let val y = (let val x = z in 3 end) val x = 4 in x + y end;" in e)),
    let Right(e) = P.parse "let val y = (let val x = fn x => x in 3 end) val x = 4 in x + y end;" in e);
   (((Int 5 , "x"),
     ( If ( Bool ( true ) , Var "x", Var "y"))), 
    If ( Bool true , Int 5 , Var "y") )*) 
] 

(* Q3  : Substitute a variable *)
let rec subst ((e', x) : exp * name) (e : exp) : exp =
  match e with
  | Var y ->
      if x = y then
        e'
      else
        Var y

  | Int _ | Bool _ -> e
  | Primop (po, es) -> Primop (po, List.map (subst (e', x)) es)
  | If (e1, e2, e3) -> If (subst (e', x) e1, subst (e', x) e2, subst (e', x) e3)
  | Tuple es -> Tuple (List.map (subst (e', x)) es)
  | Anno (e, t) -> Anno (subst (e', x) e, t)
(*MUST READ: Although I can remove the non-exhaustive pattern matching by
using match ... with, the code would end up looking convoluted and hard to read,
additionally I would prefer to seperate my concerns for easier debugging.
It is guaranteed that the other possible cases in the non-exhaustive pattern matching
will not occur, since the following Let case will only return Let expressions.*)
  | Let (ds, e2) -> (match ds with 
      | [] -> Let (ds, subst (e',x) e2) 
      | (Val (e1, nm))::decs -> if x = nm || List.mem nm (free_vars (e')) then
            let newName = fresh_var nm in
            let Let (dcs, er) = subst (Var newName, nm) (Let(decs, e2)) in (*For non-exhaustive pattern matching, 
                                                                           I know for certain the form of the result.
                                                                           Will i still lose marks ?*)
            let Let (dcs2, er2) = subst (e',x) (Let(dcs, er)) in
            Let((Val (subst (e',x) e1, newName))::dcs2, er2)
          else
            let Let (dcs, er) = subst (e',x) (Let(decs, e2)) in 
            Let ((Val (subst (e',x) e1, nm))::dcs, er) 
      | (Valtuple (e1, nmL))::decs -> 
          if List.mem x nmL || List.exists (fun nm -> List.mem nm (free_vars e')) (nmL) then
            let newNames = List.map (fresh_var) nmL in
            let Let (dcs, er) = List.fold_left2 (fun ex oldNm newNm ->
                subst (Var newNm, oldNm) ex) (Let(decs, e2)) (nmL) (newNames) in
            let Let (dcs2, er2) = subst (e',x) (Let(dcs, er)) in
            Let((Valtuple (subst (e',x) e1, newNames))::dcs2, er2)
          else
            let Let (dcs,er) = subst (e',x) (Let (decs, e2)) in
            Let ((Valtuple (subst (e', x) e1, nmL))::dcs, er)
      | (ByName (e1, nm))::decs -> if x = nm || List.mem nm (free_vars (e')) then
            let newName = fresh_var nm in
            let Let(dcs, er) = subst (Var newName, nm) (Let(decs, e2)) in 
            let Let(dcs2, er2) = subst (e',x) (Let(dcs, er)) in
            Let(ByName(subst (e',x) e1, newName)::dcs2, er2)
          else
            let Let(dcs, er) = subst (e',x) (Let(decs, e2)) in 
            Let (ByName(subst (e',x) e1, nm)::decs, er))
  | Apply (e1, e2) -> Apply (subst (e', x) e1, subst (e', x) e2)
      
  | Fn (y, t, e) -> 
      if y = x || List.mem y (free_vars e') then 
        let newvar = fresh_var y in
        let newe = subst (Var newvar, y) (e) in
        Fn (newvar, t, subst (e',x) newe)
      else
        Fn (y, t, subst (e',x) e) 
  | Rec (y, t, e) -> (*for recursion, does y have to match with the name in
                     the Val() when we substitute?*)
      if y = x || List.mem y (free_vars e') then 
        let newvar = fresh_var y in
        let newe = subst (Var newvar, y) (e) in
        Rec (newvar, t, subst (e',x) newe)
      else
        Rec (y, t, subst (e',x) e)
      
;;


let eval_tests : (exp * exp) list = [
  (* (match P.parse valid_program_1 with
        |Right(e) -> (e, Int(300))
        |Left(e) -> type_fail "Invalid Expression"); 
    (match P.parse valid_program_3 with
     |Right(e) -> (e, Int(120))
     |Left(e) -> type_fail "Invalid Expression"); 
    (match P.parse valid_program_5 with
     |Right(e) -> (e, Int(6))
     |Left(e) -> type_fail "Invalid Expression");
    (match P.parse valid_program_6 with
     |Right(e) -> (e, Int(6))
     |Left(e) -> type_fail "Invalid Expression");
    (match P.parse valid_program_7 with
     |Right(e) -> (e, Int(4))
     |Left(e) -> type_fail "Invalid Expression");
    (match P.parse valid_program_8 with
     |Right(e) -> (e, Int(900))
     |Left(e) -> type_fail "Invalid Expression");
    (match P.parse valid_program_9 with
     |Right(e) -> (e, Int(1600))
     |Left(e) -> type_fail "Invalid Expression");
    (match P.parse valid_program_10 with
     |Right(e) -> (e, Int(10))
     |Left(e) -> type_fail "Invalid Expression");
    (let Right(e) = P.parse "let val x = true in false && x end;" in e, Bool(false));
    (let Right(e) = 
       P.parse ("let val x = (let fun apply (f : int -> int) : int -> int = fn x : int => f(x) in apply (fn x => x * 3) 100 end) in let val y = 4 in x + 5 end end;") in e,
                                                                                                                                                                         Int(305));
    (let Right(e) = P.parse ("let val x = 3 in let val x = (let val x = 1 in x + 5 end) in x + x end end;") in e,
                                                                                                               Int(12));
    (let Right(e) = P.parse "let fun test (x : int) : int = y in 4 end;" in e,
                                                                            Int(4));
    (let Right(e) = P.parse "let val x = 3 val (y1,y2) = (3,4) in x + y1 end;" in e, 
                                                                                  Int(6) );
    (let Right(e) = P.parse "let val y = (let val x = fn x => x in true end) val x = false in x || y end;" in e,
                                                                                                     Bool(true)) *)
]

(* Q4  : Evaluate an expression in big-step *)
let rec eval : exp -> exp =
  (* do not change the code from here *)
  let bigstep_depth = ref 0 in
  fun e ->
    if !debug >= 1 then
      print_endline
        (String.make (!bigstep_depth) ' '
         ^ "eval (" ^ Print.exp_to_string e ^ ")\n");
    incr bigstep_depth;
  (* to here *)
    let result =
      match e with
      | Int _ | Bool _ -> e
      | Tuple es -> Tuple (List.map eval es)
      | If (e1, e2, e3) ->
          begin match eval e1 with
            | Bool b ->
                if b then
                  eval e2
                else
                  eval e3
            | _ -> stuck "Condition for if expression should be of the type bool"
          end
      | Anno (e, _) -> eval e     (* types are ignored in evaluation *)
      | Var x -> stuck ("Free variable \"" ^ x ^ "\" during evaluation")

      | Fn (x, t, e1) -> (*e1 was named e, overshadowing the above e, intentional?*)
          e
      | Apply (e1, e2) -> 
          begin match eval e1 with
            |  Fn (x,t,e3) -> 
                let v2 = eval e2 in eval (subst (v2,x) e3)
            | _ -> stuck "Can't apply an expression that is not a function to a value." 
          end
      | (Rec (f, t, e)) as re -> 
          eval (subst (re, f) e) 
      | Primop (And, es) ->
          let vs = List.map eval es in
          begin match vs with
            | [Bool b1; Bool b2] -> Bool (b1 && b2)
            | _ -> stuck "Bad arguments to primitive operation"
          end
      | Primop (Or, es) ->
          let vs = List.map eval es in
          begin match vs with
            | [Bool b1; Bool b2] -> Bool (b1 || b2)
            | _ -> stuck "Bad arguments to primitive operation"
          end
      | Primop (op, es) ->
          let vs = List.map eval es in
          begin match eval_op op vs with
            | None -> stuck "Bad arguments to primitive operation"
            | Some v -> v
          end

      | Let (ds, e) -> match ds with
        | [] -> eval e
        | (Val (e1, nm))::decs -> eval (subst (eval e1,nm) (Let(decs, e)))
        | (ByName (e1,nm))::decs -> eval (subst (e1,nm) (Let(decs, e)))
        | (Valtuple(e1,nmL))::decs -> (match eval e1 with
            | Tuple (tupleValues) -> if List.length tupleValues = List.length nmL then
                  eval (List.fold_left2 (fun ex value name -> 
                      subst (value, name) (ex)) (Let(decs,e)) (tupleValues) (nmL))
                else stuck "Tuple variable size doesn't match tuple value size."
            | _ -> stuck "Tuple variable can't be assigned to a non-tuple expression.")
    in
  (* do not change the code from here *)
    decr bigstep_depth;
    if !debug >= 1 then
      print_endline
        (String.make (!bigstep_depth) ' '
         ^ "result of eval (" ^ Print.exp_to_string e ^ ") = "
         ^ Print.exp_to_string result ^ "\n");
  (* to here *)
    result
;;
    


let unify_tests : ((typ * typ) * unit) list = [ 
  ((TInt,TBool), ()); (*Throws error*)
  (let a = fresh_tvar () in
   (TInt,a),());
  (let a = fresh_tvar () in
   let b = fresh_tvar () in
   (TArrow(a,TInt),TArrow(TBool,b)),());
  (let a = fresh_tvar () in
   (a, TProduct([TInt;a])),()); (*Throws error.*)
  (let a = fresh_tvar () in
   let b = fresh_tvar () in
   (TArrow(a,b),TArrow(a,TInt)),());
  (let a = fresh_tvar () in
   let b = fresh_tvar () in
   (TProduct([a;b]),TProduct([TInt;a])),());
  (let a = fresh_tvar () in
   let b = fresh_tvar () in
   let c = fresh_tvar () in
   (TProduct([TArrow(a,TBool);c]),TProduct([c;TArrow(b,TInt)])),()); (*Throws error.*)
  (let a = fresh_tvar () in
   let b = fresh_tvar () in
   (TArrow(b,TArrow(a,TInt)),TArrow(b,TProduct([TBool;b]))),()); (*Throws error*) 
  (let b = fresh_tvar () in
   let c = fresh_tvar () in
   (TProduct([TArrow(TInt,b);c]),TProduct([TArrow(TInt,c);b])),())
]

(*takes input a reference and a type and returns true if the reference occurs in the type tp.*)
let rec occurs_check rf tp = match tp with
  | TInt -> false
  | TBool -> false
  | TArrow (t1, t2) -> (occurs_check rf t1) || (occurs_check rf t2)
  | TProduct (tpL) -> List.exists (occurs_check rf) (tpL)
  | TVar(rf1) -> (match !rf1 with (*Is this how you properly check ???*)
      | None -> rf == rf1
      | Some(t) -> (rf == rf1) || (occurs_check rf t))
;;
(* find the next function for Q5 *)
(* Q6  : Unify two types *)
let rec unify (ty1 : typ) (ty2 : typ) : unit = match ty1, ty2 with
  | TInt, TInt ->
      ()
  | TBool, TBool -> 
      ()
  | TArrow(t1,t2), TArrow(t3, t4) -> 
      (unify t1 t3) ; (unify t2 t4)
  | TProduct(tpL1), TProduct(tpL2) -> 
      if (List.length tpL1) = (List.length tpL2) 
      then List.iter2 (unify) (tpL1) (tpL2) 
      else type_fail "Types can't be unified."
  | TVar(rf1), ty2 -> (match !rf1, ty2 with 
      | Some(tp), _ -> unify tp ty2
      | None, TVar(sm) -> (match !sm with
          | None -> if sm == rf1 then () else rf1 := Some(ty2) (*Should we instead change the reference of 
                                                              rf1 to be the same as sm?*)
          | Some(s) -> if occurs_check rf1 ty2 then type_fail "Types can't be unified."
              else rf1 := Some(ty2)) 
          
      | None, _ -> if occurs_check rf1 ty2 then type_fail "Types can't be unified."
          else rf1 := Some(ty2) (*perform occurs_check, which checks if this reference rf1 occurs in ty2,
                if not, simply assign this reference to ty2, if yes, raise type_fail*)
    )
    
  | ty1, TVar(rf2) -> (match ty1, !rf2 with 
      | _, Some(tp) -> unify ty1 tp
      | TVar(sm), None -> (match !sm with
            None -> if sm == rf2 then () else rf2 := Some(ty1)
          | Some(s) -> if occurs_check rf2 ty1 then type_fail "Types can't be unified."
              else rf2 := Some(ty1)) 
          
      | _, None -> if occurs_check rf2 ty1 then type_fail "Types can't be unified."
          else rf2 := Some(ty1) (*perform occurs_check, which checks if this reference rf1 occurs in ty2,
                if not, simply assign this reference to ty2, if yes, raise type_fail*)
    )
  | _, _ -> type_fail "Types can't be unified."
;;                               
          
let infer_tests : ((context * exp) * typ) list = [
  (match P.parse valid_program_1 with
   |Right(e) -> ((Ctx [],e),TInt)
   |Left(e) -> type_fail "Invalid Expression");
  (match P.parse valid_program_2 with
   |Right(e) -> ((Ctx [],e),TInt)
   |Left(e) -> type_fail "Invalid Expression");
  (match P.parse valid_program_3 with
   |Right(e) -> ((Ctx [],e),TInt)
   |Left(e) -> type_fail "Invalid Expression");
  (match P.parse valid_program_4 with
   |Right(e) -> ((Ctx [],e),TInt)
   |Left(e) -> type_fail "Invalid Expression");
  (match P.parse valid_program_5 with
   |Right(e) -> ((Ctx [],e),TInt)
   |Left(e) -> type_fail "Invalid Expression");
  (match P.parse valid_program_6 with
   |Right(e) -> ((Ctx [],e),TInt)
   |Left(e) -> type_fail "Invalid Expression");
  (match P.parse valid_program_7 with
   |Right(e) -> ((Ctx [],e),TInt)
   |Left(e) -> type_fail "Invalid Expression");
  (match P.parse valid_program_8 with
   |Right(e) -> ((Ctx [],e),TInt)
   |Left(e) -> type_fail "Invalid Expression");
  (match P.parse valid_program_9 with
   |Right(e) -> ((Ctx [],e),TInt)
   |Left(e) -> type_fail "Invalid Expression");
  (match P.parse valid_program_10 with
   |Right(e) -> ((Ctx [],e),TInt)
   |Left(e) -> type_fail "Invalid Expression");
  (match P.parse "fn x => fn y => (y + 1,fn z => x);" with (*Why doesn't this give different type variables?*)
   |Right(e) -> ((Ctx [],e),TInt)
   |Left(e) -> type_fail "Invalid Expression");
  (match P.parse "if z then fn x => x else fn y => 3;" with (*Should this fail? See question below.*)
   |Right(e) -> ((Ctx [],e),TInt)
   |Left(e) -> type_fail "Invalid Expression");
  (match P.parse "(3,true);" with
   |Right(e) -> ((Ctx [],e),TProduct([TInt;TBool]))
   |Left(e) -> type_fail "Invalid Expression");
  ((Ctx [],Int(8)),TInt);
  ((Ctx [],Bool(true)),TBool);
  ((Ctx [],Tuple([Int(8);Bool(true)])),TProduct([TInt;TBool]));
  ((Ctx [],Fn("x",Some(TInt),Var "x")),TArrow(TInt,TInt));
  ((Ctx [],Let ([Val (Int 4, "x")],
                Let ([Val (Bool true, "x"); Val (Var "x", "y")], Var "y"))),TBool);
  ((Ctx [],Let ([Valtuple (Int 4, ["x"; "y"])], Var "x")),TInt); (*Should raise an exception.*)
  ((Ctx [],Apply (Fn ("x", Some TInt, Bool true), Int 4)),TBool);
  ((Ctx [],Let
      ([Val
          (Rec ("f", TArrow (TInt, TInt),
                Fn
                  ("x", Some TInt,
                   Primop (Equals,
                           [Var "x"; Apply (Var "f", Primop (Plus, [Var "x"; Int 1]))]))),
           "f")],
       Apply (Var "f", Int 3))),TInt); (*Ill-typed*)
  ((Ctx [],Fn ("x", None, Var "x")),TInt); (*The following tests use type variables and require unify.*)                              
  ((Ctx [],Apply (Fn ("x", None, Var "x"), Int 3)),TInt);
  ((Ctx [],Fn ("x", None, Primop (GreaterThan, [Var "x"; Int 3]))),TInt);
  ((Ctx [],Fn ("x", None, Fn ("y", None, Var "x"))),TInt); (*Why is it giving same type variables. and not a->b->a ?*)
  ((Ctx [],Apply (Fn ("x", None, Fn ("y", None, Var "x")), Int 3)),TInt);
  ((Ctx [],Tuple
      [Apply (Fn ("x", None, Var "x"), Int 1);
       Apply (Fn ("x", None, Var "x"), Bool true)]),TInt); 
  ((Ctx [],Let ([Val (Fn ("x", None, Var "x"), "f")],
                Tuple [Apply (Var "f", Int 1); Apply (Var "f", Bool true)])),TInt); (*Ill-typed*)
  ((Ctx [],(Fn ("x", None, Apply (Var "x", Var "x")))),TInt) (*Ill-typed*)
] (*add tests*)
                  
  (*The following code I'm adding as a helper*)
(*return (expectedArguments, returnType*)
let primopType p = match p with
  | Equals | NotEquals | LessThan | LessEqual
  | GreaterThan | GreaterEqual -> ([TInt;TInt], TBool)
  | Plus | Minus | Times | Div -> ([TInt;TInt], TInt)
  | Negate -> ([TInt], TInt)
  | And | Or -> ([TBool;TBool], TBool) 
;;
(*******************************************)
(*Note: There was a bug in the Pretty Printer that didnt check references using ==,
pls make sure when you test infer, this bug is updated. Or else all type variables will
look the same.*)   
(* Q5  : Type an expression *)
(* Q7* : Implement the argument type inference
         For this question, move this function below the "unify". *)
(*DONT FORGET TO MAKE SURE ALL EXPRESSIONS HAVE THEIR CONSTRAINTS!!!!!!!!!!!!!!!!*)
let rec infer (ctx : context) (e : exp) : typ = match e with
  | Var x -> ctx_lookup ctx x
  | Int _ -> TInt
  | Bool _ -> TBool
  | If (e,e1,e2) -> 
      unify (infer ctx e) TBool; 
      let t1 = infer ctx e1 in
      let t2 = infer ctx e2 in 
      unify t1 t2 ; t1
  | Fn (x, t, e1) -> (match t with
      | Some (funType) ->  
          let t' = infer (extend ctx (x, funType)) e1 in TArrow (funType, t')
      | None -> let frshtype = fresh_tvar () in 
          let t' = infer (extend ctx (x, frshtype)) e1 in TArrow (frshtype, t'))
  | Tuple es -> TProduct (List.map (infer ctx) es)
  | Rec (f, t, e) -> 
      let inferdType = infer (extend ctx (f, t)) e in
      unify inferdType t; inferdType 
  | Apply (e1, e2) -> 
      let e2Type = infer ctx e2 in 
      (match infer ctx e1 with
       | TArrow(t1, t') -> unify t1 e2Type; t'
       | _ -> type_fail "First argument is not a function, or doesn't have same input type as second argument."
      )
  | Anno (e1, t1) -> unify (infer ctx e1) t1; t1
  | Primop (po, expL) -> 
      let (expectedArgTypes, resultType) = primopType po in
      let inferredArgTypes = List.map (infer ctx) expL in
      let rec compare tlist1 tlist2 = match tlist1, tlist2 with
        | [] , [] -> resultType
        | t::tlist, s::slist -> 
            unify t s; compare tlist slist
        | _ , _ -> type_fail "Incorrect number of arguments supplied to Primitive operator."
      in
      compare expectedArgTypes inferredArgTypes
  | Let(ds, e1) -> match ds with(*Last but not least.*)
    | [] -> infer ctx e1
    | (Val (e2, nm))::decs -> infer (extend ctx (nm,infer ctx e2)) (Let(decs, e1))
    | (ByName (e2,nm))::decs ->  infer (extend ctx (nm,infer ctx e2)) (Let(decs, e1))
    | (Valtuple (e2,nmL))::decs -> (match infer ctx e2 with
          TProduct (tupleTypes) -> if List.length tupleTypes = List.length nmL then
              infer (extend_list ctx (List.map2 (fun name ty -> (name, ty)) (nmL) (tupleTypes))) (Let (decs, e1))
            else type_fail "Not enough arguments for tuple. Syntax is incorrect."
        | _ -> type_fail "Types can't be inferred, expected TProduct but inferred something else.") 
;;

(* Now you can play with the language that you've implemented! *)
let execute (s: string) : unit =
  match P.parse s with
  | Left s -> print_endline ("parsing failed: " ^ s)
  | Right e ->
      try
       (* first we type check the program *)
        ignore (infer (Ctx []) e);
        let result = eval e in
        print_endline ("program is evaluated to: " ^ Print.exp_to_string result)
      with
      | NotImplemented -> print_endline "code is not fully implemented"
      | Stuck s -> print_endline ("evaluation got stuck: " ^ s)
      | NotFound -> print_endline "variable lookup failed"
      | TypeError s -> print_endline ("type error: " ^ s)
      | e -> print_endline ("unknown failure: " ^ Printexc.to_string e)


(************************************************************
 *             Do not change these functions.               *
 *               They are needed for tests.                 *
 ************************************************************)
let list_to_string el_to_string l : string =
  List.fold_left
    begin fun acc el ->
      if acc = "" then
        el_to_string el
      else
        acc ^ "; " ^ el_to_string el
    end
    ""
    l
  |> fun str -> "[" ^ str ^ "]"

let run_test name f ts stringify : unit =
  List.iteri
    begin fun idx (input, expected_output) ->
      try
        let output = f input in
        if output <> expected_output then
          begin
            print_string (name ^ " test #" ^ string_of_int idx ^ " failed\n");
            print_string (stringify output ^ " \n<>\n " ^ stringify expected_output ^ "\n\n")
          end
      with
      | exn ->
          print_string (name ^ " test #" ^ string_of_int idx ^ " raised an exception:\n");
          print_string (Printexc.to_string exn)
    end
    ts

let run_free_vars_tests () : unit =
  run_test "free_vars" free_vars free_vars_tests (list_to_string (fun x -> x))

let run_unused_vars_tests () : unit =
  run_test "unused_vars" unused_vars unused_vars_tests (list_to_string (fun x -> x))

let run_subst_tests () : unit =
  run_test "subst" (fun (s, e) -> subst s e) subst_tests Print.exp_to_string

let run_eval_tests () : unit =
  run_test "eval" eval eval_tests Print.exp_to_string

let run_infer_tests () : unit =
  run_test "infer" (fun (ctx, e) -> infer ctx e) infer_tests Print.typ_to_string

let run_unify_tests () : unit =
  run_test "unify" (fun (ty1, ty2) -> unify ty1 ty2) unify_tests (fun () -> "()")

let run_all_tests () : unit =
  run_free_vars_tests ();
  run_unused_vars_tests ();
  run_subst_tests ();
  run_eval_tests ();
  run_infer_tests ();
  run_unify_tests ()
