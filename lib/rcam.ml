(** Lambda bound variables **)
type var = { mutable dummy : unit }  (* only their memory address count;
                                        mutable is used to avoid common
                                        subexpression elimination *)

(** Plotkin's terms **)
type term =
 | Var of var
 | Lam of var * term
 | App of term * term

(** Finest crumbled terms **)
type node =                       (* nodes are explicit substitutions *)
 { mutable content : bite         (* the substituted term *)
 ; mutable copying : node option  (* used only during copying to preserve sharing *)
 ; mutable prev : node_list       (* pointer to the next subst in the environment *)
 }    
and node_list = node option       (* a list of nodes is an environment *)
and name = V of var | N of node   (* a name is either a lambda-bound variable or the address of a node *)
and bite =
 | Lam1 of var * crumbp
 | Lam2 of var * name
 | VV of name * name
and crumbp = bite * env           (* (b,e) encodes the crumble [b/*]e *)
and env = (node * node) option    (* to simplify the code we keep two pointers to the first
                                     and the last substitution in the environments used in the
                                     bodies of lambda-abstractions *)

(** Reversible Crumbling Machine states **)
type history_entry =             
 | Search
 | Principal of node * node

type machine_state =
 (* in a machine state (E1, Ev, H) the pair of environments
    (E1,Ev) implements a Zipper data structure; in particular
    cells in E1 point to their left and cells in Ev point to
    their right *)
 node_list * node_list * history_entry list

(** Reversible Crumbling Machine computation direction **)
type eval_direction = F | B       (* Forward vs Backward *)

(**************** utilities ******************)

let push n e =
 n.prev <- Some e

(* moves the pointer to the left in a zipper;
   n  = head of the non-empty node_list to the left
   ev = node_list to the right
   returns the pair of node_lists
*)
let move_left n ev =
 let p = n.prev in
 n.prev <- ev ;
 (p,Some n)

(* moves the pointer to the right in a zipper
   n = node_list to the left
   ev = node_list to the right, assumed to be non-empty
   returns the head of the left node_list and the right node_list 
*)
let move_right n ev =
 match ev with
   None ->
    (* the input state is not reachable! *)
    assert false
 | Some m ->
    let ev = m.prev in
    m.prev <- n ;
    m,ev

let mk_node content = { content ; copying = None ; prev = None }
let mk_var () = { dummy = () }  (* it creates a fresh variable *)


(**************** pretty printing ******************)

(*
   \027[0m    normal
   \027[4m    underline
   \027[0;31m red
   \027[32m   green
   \027[35m   purple
*)
let normal_color = "\027[0m"
let ev_color = "\027[32m"
let code_color = "\027[0;35m"
let history_color = "\027[4m"

let (^^) s1 s2 =
 if s1 = "" then s2 else
  if s2 = "" then s1 else
   s1 ^ " " ^ s2

let color c s =
 if s = "" then s else c ^ s ^ normal_color

let string_of_it ?star s =
 let m0 = match star with None -> [] | Some v -> [(v,"*")] in
 let c = ref 1 in
 let m = ref m0 in
 (function () -> c := 1; m := m0),
 (function v ->
  try List.assq v !m
  with Not_found ->
   let n = s ^ Utils.render_subscript !c in
   incr c ;
   m := (v,n)::!m ;
   n)

let dummy = VV (V (mk_var ()), V (mk_var ()))
let star = mk_node dummy (* the variable printed as * *)

let reset_var,string_of_var = string_of_it "x"
let reset_node,string_of_node = string_of_it ~star "y"

(* resets the counters for fresh names *)
let reset () = reset_var () ; reset_node ()

let rec string_of_t =
 function
    App(t1,t2) ->
     let s1 = string_of_t t1 in
     let s2 = string_of_t t2 in
     "(" ^ s1 ^ s2 ^ ")"
  | Lam(v,c) ->
     let s1 = string_of_var v in
     let s2 = string_of_t c in
      "(λ" ^ s1 ^ "." ^ s2 ^ ")"
  | Var v -> string_of_var v

let string_of_name =
 function
    V v -> string_of_var v
  | N n -> string_of_node n

let rec string_of_bite =
 function
  | Lam1(v,c) -> "λ" ^ string_of_var v ^ "." ^ string_of_crumbp c
  | Lam2(v,n) -> "λ" ^ string_of_var v ^ ".[" ^ string_of_name n ^ "/*]"
  | VV (v1,v2) ->
     let s1 = string_of_name v1 in
     let s2 = string_of_name v2 in
     s1 ^ s2

and string_of_crumbp (t,e) =
 "[" ^ string_of_bite t ^ "/*]" ^
 match e with
    None -> ""
  | Some (_,n) -> pretty_print_to n

and pretty_print_subst v =
 "[" ^ string_of_bite v.content ^ "/" ^ string_of_node v ^ "]"

and pretty_print_prev v =
 match v.prev with
    None -> ""
  | Some v' -> pretty_print_to v'

and pretty_print_to v =
  pretty_print_prev v ^
  pretty_print_subst v

let rec pretty_print_code =
 function
    None -> ""
  | Some n ->
     let s1 = pretty_print_code n.prev in
     let s2 = pretty_print_subst n in
     s1 ^^ s2

let rec pretty_print_ev =
 function
    None -> ""
  | Some n ->
     pretty_print_subst n ^^ pretty_print_ev n.prev

let pretty_print_history_entry =
 function
    Search -> "<>"
  | Principal(v1,v2) -> "<" ^ string_of_node v1 ^ "," ^ string_of_node v2 ^ ">"

let pretty_print_history h =
 "H=" ^ String.concat ":" (List.map pretty_print_history_entry h)

let pretty_print_state (n,ev,h) =
 color code_color (pretty_print_code n) ^^
 color ev_color (pretty_print_ev ev) ^^
 color history_color (pretty_print_history h)


(**************** read-back ******************)

let rec read_back_name =
 function
  | V v -> Var v
  | N n -> read_back_bite n.content
and read_back_bite =
 function
  | VV(n1,n2) -> App(read_back_name n1, read_back_name n2)
  | Lam1(v,(b,_)) -> Lam(v, read_back_bite b)
  | Lam2(v,n) -> Lam(v, read_back_name n)

let rec last n =
 match n.prev with
  | None -> n
  | Some n -> last n

let read_back (n,ev,_) =
 match n,ev with
  | None, None -> assert false (* state is not reachable *)
  | None, Some n -> read_back_bite n.content
  | Some n, _ -> read_back_bite ((last n).content)

(**************** reduction ******************)

(* machine initialization from a crumbled term *)
let init t = (Some t, None, [])

(* the copying i.e. alpha-conversion algorithm *)
let copying_node y y' f =
  y.copying <- Some y' ;
  let res = f () in
  y.copying <- None ;
  res

let copy_crumbp v t p =
 let rec copy = function
  | VV(n1,n2) -> VV(copy_name n1,copy_name n2)
  | Lam1(v,e) -> Lam1(v,copy_crumbp e)
  | Lam2(v,n) -> Lam2(v,copy_name n)
 and copy_name = function
  | V v' when v == v' -> t
  | N {copying=Some b} -> N b
  | n -> n
 and copy_env c e =
  let n' = mk_node (copy e.content) in
  copying_node e n' (fun () ->
  match e.prev with
   | None -> copy c, n', n'
   | Some prev ->
      let c',b',e' = copy_env c prev in
      push n' e' ;
      c',b',n')
 and copy_crumbp (c,e) =
  match e with
     None -> copy c, None
   | Some (b,e) -> let c',b',e' = copy_env c e in c', Some (b',e')
 in copy_crumbp p

(* auxiliary functions used for reduction *)
let (@) n (c,env) =
 n.content <- c ;
 match env with
    None -> n
  | Some (b,e) -> push b n ; e

let rec length =
 function
  | None -> 0
  | Some (_,e) -> 1 + aux e.prev
and aux =
 function
  | None -> 0
  | Some e -> 1 + aux e.prev

let step s =
 print_string ("-->" ^ s ^ " ")

let rec drop len n =
 match n with
 | None -> assert false (* state is not reachable *)
 | Some m ->
    if len = 0 then m else drop (len-1) m.prev

let subst_var y n =
 function
  | V x when x == y -> n
  | V _ -> raise (Failure "term not closed!")
  | N n -> n

(* the evaluation function
   - dir is the reduction direction
   to show that both directions work, the machine goes forward
   until the normal form is found; then it reverts the direction
   until it reaches the initial crumble, i.e. the backward normal form
*)
let rec eval dir ((n,ev,h) as state : machine_state) =
 print_endline (pretty_print_state state) ;
 print_string ("read-backs to " ^ string_of_t (read_back state) ^ "\n") ;
 match dir with
 | F -> 
   (match n with
     | None ->
        (* normal form reached *)
        print_string "final state reached\n\n" ;
        print_string "reverting the computation\n       " ;
        eval B (n,ev,h)
     | Some n ->
        match n.content with
         | VV(N ({content=Lam1(y,c)} as v1), (N v2 as t)) -> 
            step "m₁ " ;
            let c' = copy_crumbp y t c in
            eval dir (Some (n @ c'),ev,Principal(v1,v2)::h)
         | VV(N ({content=Lam2(y,z)} as v1), N({content=t} as v2)) -> 
            step "m₂ " ;
            n.content <- (subst_var y v2 z).content ;
            let n,ev = move_left n ev in
            eval dir (n,ev,Principal(v1,v2)::h)
         | Lam1 _ | Lam2 _ ->
            step "sea" ;
            let n,ev = move_left n ev in
            eval dir (n,ev,Search::h)
         | VV (_, V _) ->
            raise (Failure "term not closed!")
         | VV (N {content=VV _}, _) | VV (V _, _) ->
            (* non reachable state *)
            assert false)
 | B ->
   (match h with
     | [] ->
        (* Initial state reached going backward *)
        print_endline "initial state reached again"
     | Principal({content=Lam1(_,(_,env))} as v1,v2)::h ->
        step "m₁ᵇ" ;
        let len = length env in
        let m = drop len n in
        m.content <- VV(N v1, N v2) ;
        eval dir (Some m,ev,h)
     | Principal({content=Lam2(_,_)} as v1,v2)::h ->
        step "m₂ᵇ" ;
        let n,ev = move_right n ev in
        n.content <- VV(N v1, N v2) ;
        eval dir (Some n,ev,h)
     | Search::h ->
        step "seaᵇ" ;
        let n,ev = move_right n ev in
        eval dir (Some n,ev,h)
     | Principal({content=VV _},_)::_ ->
        (* the input state is not reachable! *)
        assert false)

(********* finest crumbling traslation ***************)

let anf t =
 let rec aux_term c e = match c with
 | App (Var v1, Var v2) -> VV(V v1, V v2), e
 | App(Var v, w) ->
    let w, e = aux_es w e in 
    VV(V v, N w), e
 | App(v, Var w) ->
    let v, e = aux_es v e in 
    VV(N v, V w), e
 | App(v, w) ->
    let v, e = aux_es v e in 
    let w, e = aux_es w e in 
    VV(N v, N w), e
 | Lam(x, Var y) -> Lam2(x, V y), e
 | Lam(x, t) -> Lam1(x, aux_term t None), e
 | Var _ ->
    raise (Failure "Not a closed term")
 and aux_es c e =
  let n = mk_node dummy in
  let b =
   (match e with
       None -> n
     | Some (b,e) -> push n e ; b) in
  let c, e = aux_term c (Some (b,n)) in
  n.content <- c;
  n, e
 in
  star @ (aux_term t None)

let translate = anf ;;

(** evaluation of Plotkin's terms via crumbling + machine reduction  **)

let evaluate t =
 let crumble = translate t in
 print_endline ("term to evaluate\n       " ^ string_of_t t);
 print_endline ("\nits finest crumbled form is\n       " ^ pretty_print_code (Some crumble)) ;
 print_string  ("\ninitial machine state\n       ") ;
 eval F (init crumble) ;
 reset () ;
 print_endline "\n"

(** Convert a Syntax_tree.term in a Plotkin's term **)

exception UnboundVariable of string

let rec scope_checker env (t: Syntax_tree.term): term =
   match t with
    | Var v -> (match List.assoc_opt v env with
                  | Some vref -> Var vref
                  | None -> raise (UnboundVariable v))
    | Abs (v, body) ->
       let vref = mk_var () in
       Lam (vref, scope_checker ((v, vref) :: env) body)
    | App (head, arg) -> App (scope_checker env head, scope_checker env arg)

let scope_check t = scope_checker [] t
