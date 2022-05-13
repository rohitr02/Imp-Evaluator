open Ast

exception TypeError
exception UndefinedVar
exception DivByZeroError

(* Remove shadowed bindings *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Env print function to stdout *)
let print_env_std (env : environment): unit =
  List.fold_left (fun _ (var, value) ->
      match value with
        | Int_Val i -> Printf.printf "- %s => %s\n" var (string_of_int i)
        | Bool_Val b -> Printf.printf "- %s => %s\n" var (string_of_bool b)
        | Closure _ -> ()) () (prune_env env)

(* Env print function to string *)
let print_env_str (env : environment): string =
  List.fold_left (fun acc (var, value) ->
      match value with
        | Int_Val i -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_int i))
        | Bool_Val b -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_bool b))
        | Closure _ -> acc
      ) "" (prune_env env)


(***********************)
(****** Your Code ******)
(***********************)

(* evaluate an arithmetic expression in an environment *)
let rec eval_expr (e : exp) (env : environment) : value = 
    match e with
      | Number i -> Int_Val i
      | True -> Bool_Val (true)
      | False -> Bool_Val (false)
      | Var id -> (try List.assoc id env with Not_found -> raise(UndefinedVar))
      | Plus (e1, e2) -> 
        let v1 = eval_expr e1 env in
        let v2 = eval_expr e2 env in
        (match v1, v2 with 
          | Int_Val i1, Int_Val i2 -> Int_Val (i1 + i2)
          | _ -> raise(TypeError))
      | Minus (e1, e2) -> 
        let v1 = eval_expr e1 env in
        let v2 = eval_expr e2 env in
        (match v1, v2 with 
          | Int_Val i1, Int_Val i2 -> Int_Val (i1 - i2)
          | _ -> raise(TypeError))
      | Times (e1, e2) -> 
        let v1 = eval_expr e1 env in
        let v2 = eval_expr e2 env in
        (match v1, v2 with 
          | Int_Val i1, Int_Val i2 -> Int_Val (i1 * i2)
          | _ -> raise(TypeError))
      | Div (e1, e2) -> 
        let v1 = eval_expr e1 env in
        let v2 = eval_expr e2 env in
        (match v1, v2 with 
          | Int_Val i1, Int_Val i2 -> 
            if i2 = 0 then raise(DivByZeroError) else Int_Val (i1 / i2)
          | _ -> raise(TypeError))
      | Mod (e1, e2) -> 
        let v1 = eval_expr e1 env in
        let v2 = eval_expr e2 env in
        (match v1, v2 with 
          | Int_Val i1, Int_Val i2 -> 
            if i2 = 0 then raise(DivByZeroError) else Int_Val (i1 mod i2)
          | _ -> raise(TypeError))
      | Or (e1, e2) -> 
        let v1 = eval_expr e1 env in
        let v2 = eval_expr e2 env in
        (match v1, v2 with 
          | Bool_Val b1, Bool_Val b2 -> Bool_Val (b1 || b2)
          | _ -> raise(TypeError))
      | And (e1, e2) -> 
        let v1 = eval_expr e1 env in
        let v2 = eval_expr e2 env in
        (match v1, v2 with 
          | Bool_Val b1, Bool_Val b2 -> Bool_Val (b1 && b2)
          | _ -> raise(TypeError))
      | Not (e1) -> 
        let v1 = eval_expr e1 env in
        (match v1 with 
          | Bool_Val b1 -> Bool_Val (not b1)
          | _ -> raise(TypeError))
      | Lt (e1, e2) -> 
        let v1 = eval_expr e1 env in
        let v2 = eval_expr e2 env in
        (match v1, v2 with 
          | Int_Val i1, Int_Val i2 -> Bool_Val (i1 < i2)
          | _ -> raise(TypeError))
      | Leq (e1, e2) -> 
        let v1 = eval_expr e1 env in
        let v2 = eval_expr e2 env in
        (match v1, v2 with 
          | Int_Val i1, Int_Val i2 -> Bool_Val (i1 <= i2)
          | _ -> raise(TypeError))
      | Eq (e1, e2) -> 
        let v1 = eval_expr e1 env in
        let v2 = eval_expr e2 env in
        (match v1, v2 with 
          | Int_Val i1, Int_Val i2 -> Bool_Val (i1 = i2)
          | Bool_Val b1, Bool_Val b2 -> Bool_Val (b1 = b2)
          | _ -> raise(TypeError))
      | App (e1, e2) -> 
        let v1 = eval_expr e1 env in
        let v2 = eval_expr e2 env in
        (match v1 with 
          | Closure (env, x, e) -> eval_expr e ((x, v2) :: env)
          | _ -> raise(TypeError))
      | Fun (id, e) -> Closure (env, id, e)


(* lookup function to search for x in list containing elements of the form (x,y) *)
let rec lookup (x : string) (l : (string * value)list) : (string * value) =
    match l with
      | (x1, v1) :: l2 -> if x = x1 then (x1, v1) else lookup x l2
      | _ -> raise(UndefinedVar)


(* evaluate a command in an environment *)
let rec eval_command (c : com) (env : environment) : environment =
    match c with
      | Skip -> env
      | Comp (c1, c2) -> eval_command c2 (eval_command c1 env)
      | Declare (t, x) -> (match t with
          | Int_Type -> (x, Int_Val 0) :: env
          | Bool_Type -> (x, Bool_Val false) :: env
          | Lambda_Type -> (x, Closure (env, "x", Var "x")) :: env)
      | Assg (x, e) -> 
        let v = eval_expr e env in
        (match lookup x env with (x1, v1) -> 
            match v, v1 with
              | Int_Val i, Int_Val j -> (x1, Int_Val i) :: env
              | Bool_Val b1, Bool_Val b2 -> (x1, Bool_Val b1) :: env
              | Closure (a1, b1, c1), Closure (a2, b2, c2) -> (x1, Closure (a1, b1, c1)) :: env
              | _ -> raise(TypeError))
      | Cond (e, c1, c2) -> (match eval_expr e env with
          | Bool_Val b -> if b then eval_command c1 env else eval_command c2 env
          | _ -> raise TypeError)
      | While (e, c) -> (match eval_expr e env with 
          | Bool_Val b -> if b then eval_command (Comp (c, (While (e, c)))) env else eval_command Skip env
          | _ -> raise TypeError)
      | For (e, c) -> (match eval_expr e env with 
          | Int_Val i -> if i > 0 then eval_command (Comp (c, (For (Number(i-1), c)))) env else eval_command Skip env
          | _ -> raise TypeError)