open Format;;
open Ff;;

let thm_play_count = ref 0;;
let play_count = ref 2000;;
let one_per_play = ref true;;
let ucb_mode = ref 0;;
let do_ucb = ref true;;
let play_dep = ref 1000;;
let ucb_const = ref 1.;;
let value_factor = ref 0.3;;
let save_above = ref (-1);;
let predict_value = ref true;;
let predict_policy = ref true;;
let policy_temp = ref 2.;;

let max_time = ref 60.;;
let max_mem = ref 3000000;;
let max_infs = ref 20000000;;

type tree_kind = Open | Unexplored | Lost | Won;;
type tree = {
  mutable kind : tree_kind;
  p : float;            (* prediction value *)
  mutable w : float;    (* W = wins *)
  mutable n : int;      (* N = visit count *)
  mutable b : tree list;(* subtrees for actions *)
  mutable r : float;
};;
(* Shortest proof so far *)
let is_theorem = ref [];;
(* Main history otherwise *)
let bigstep_trees = ref [];;

(* Used in the interactive mode and in "-debug" *)
let rec print_tree more fmt t =
  let kind2string = function Lost -> "F" | Won -> "W" | Unexplored -> "U" | Open -> "o" in
  if t.kind = Open && more > 0 then pp_open_box fmt 2;
  fprintf fmt "%s " (kind2string t.kind);
  fprintf fmt "%.2f (%.6f %d)" t.p (t.w /. float t.n) t.n;
  if t.kind = Open then
    if more > 0 then begin
      pp_print_string fmt " ["; pp_force_newline fmt ();
      pp_print_list ~pp_sep:(fun f () -> pp_print_string f "; "; pp_force_newline fmt ())
                    (print_tree (more - 1)) fmt t.b;
      pp_print_char fmt ']';
      pp_close_box fmt ()
    end else pp_print_string fmt " ...";;
let print_tree2 = print_tree 2;;

let rec splitl a1 a2 = function
  | h1 :: h2 :: t -> splitl (h1 :: a1) (h2 :: a2) t
  | [h] -> if Random.int 2 = 0 then (a1, h :: a2) else (h :: a1, a2)
  | [] -> (a1, a2)

let nnumber = ref 0;;
let rec print_dot abbr parent fmt t =
  if t.kind = Open && List.length t.b = 1 then print_dot (abbr + 1) parent fmt (List.hd t.b) else begin
    incr nnumber;
    let no = !nnumber in
    let kind2string = function Lost -> "style=filled color=red" | Won -> "style=filled color=blue" | Unexplored -> "style=filled shape=circle color=gray" | Open -> "" in
    fprintf fmt "n%i [%s %s label=\"p=%.2f" no (kind2string t.kind) (if List.memq t !is_theorem then "penwidth=5" else "") t.p;
    if t.kind = Open then fprintf fmt "\\na=%.4f\\nr=%.4f\\nv=%d" (t.w /. float t.n) t.r t.n;
    fprintf fmt "\"]; n%i->n%i %s;\n" parent no (if abbr < 2 then "" else "[label=\"STEPS:" ^ string_of_int abbr ^ "\"]");
    let notu,ub = List.partition (fun t -> t.kind <> Unexplored) t.b in
    let ubfst = true in
    if ubfst && ub <> [] then
      if List.tl ub <> [] then fprintf fmt "n%iu [style=filled color=gray label=\"..%i..\" shape=circle]; n%i->n%iu;\n" no (List.length ub) no no
      else print_dot 1 no fmt (List.hd ub);
    (*if t.kind = Open then *)
(*    let notu1, notu2 = List.partition (fun t -> List.mem t !is_theorem) notu in
    let notu0, notu2 = splitl [] [] notu2 in*)
    List.iter (print_dot 1 no fmt) notu (*List.rev_append notu0 (List.rev_append notu1 notu2)*);
(*    if not ubfst && ub <> [] then
      if List.tl ub <> [] then fprintf fmt "n%iu [style=filled color=gray label=\"..%i..\" shape=circle]; n%i->n%iu;\n" no (List.length ub) no no
      else print_dot 1 no fmt (List.hd ub);*)
  end
;;

let print_dot t =
  let oc = open_out "x.g" in
  let f = Format.formatter_of_out_channel oc in
  Format.fprintf f "digraph Q {\nnode [shape=record];\nn0;\n";
  print_dot 1 0 f t;
  Format.fprintf f "}\n%!";
  close_out oc;;


(* Some counters *)
let bigsteps, opened, closed, edges, totfea = ref 0, ref 0, ref 0, ref 0, ref 0;;

Xgb.init (!predict_policy || !predict_value);;

let priors ((p, _), _) actions =
  edges := !edges + List.length actions;
  match !predict_policy with
  | false ->
     List.map (fun _ -> 1.) actions
  | true ->
     fea_global_update p;
     let fealist = List.map (get_act_fea p) actions in
     List.iter (fun f -> totfea := !totfea + List.length f) fealist;
     let predicts = Xgb.predict_p fealist in
     List.map (fun p -> exp (p /. !policy_temp)) predicts;;

(* Initial tree with one unexplored node *)
let itree = {kind=Unexplored; p=1.; w=0.; n=0; b=[]; r=0.};;

let normalize l =
  let sum = List.fold_left (+.) 0. l in
  List.map (fun v -> v /. sum) l;;

let do_tree tree thist (i, (st, acts)) =
  let fail tree = if tree.kind <> Lost then begin incr closed; tree.kind <- Lost end in
  match i with
  | 1 -> tree.kind <- Won;
         if !is_theorem = [] || List.length !is_theorem > List.length thist + 1 then is_theorem := tree :: thist;
         max_infs := 1000000000;
         if !thm_play_count >= 0 then play_count := !thm_play_count;
         (st, tree, tree :: thist, [])
  | -1 -> fail tree; (st, tree, tree :: thist, [])
  | _ ->
     if acts = [] then (fail tree; (st, tree, tree :: thist, [])) else
     match tree.kind with
     | Won | Lost -> (st, tree, tree :: thist, [])
     | Open ->
        if List.exists (fun x -> x.kind <> Lost) tree.b then
          (st, tree, tree :: thist, acts)
        else
          (fail tree; (st, tree, tree :: thist, []))
     | Unexplored ->
        incr opened;
        let l = normalize (priors st acts) in
        let b = List.map (fun p -> {kind=Unexplored; p; w=0.; n=0; b=[]; r=0.}) l in
        if (not !one_per_play) then tree.kind <- Open;
        tree.b <- b;
        (st, tree, tree :: thist, acts);;

(* 'arg_max get_val l' computes the _index_ of element of list l which has maximal get_val *)
let arg_max get_val = function
  | h :: t ->
     let rec aux ind indmax maxval = function
       | [] -> indmax
       | h :: t ->
          let v = get_val h in
          if v > maxval then aux (ind + 1) ind v t else aux (ind + 1) indmax maxval t
     in aux 1 0 (get_val h) t
  | _ -> failwith "arg_max: empty list";;


let max1 a b = max a b;;
let max2 a b = max a b;;

let ucb sum_visits prior wins visits =
(*  let wins = if visits = 0 then 1.0 else wins in*)
  let visits = max1 1.0 (float visits) and sum_visits = max2 1.0 (float sum_visits) in
  let factor =
    match !ucb_mode with
    | 1 -> sqrt (sum_visits /. visits)       (* UCB no logarithm *)
    | 2 -> sqrt sum_visits /. visits         (* PUCB from Alpha Zero *)
    | _ -> sqrt ((log sum_visits) /. visits) (* Original Csaba Szepesvari *)
  in
  if !do_ucb then (wins /. visits) +. !ucb_const *. prior *. factor else Random.float 1. *. prior;;

let get_rel sum_visits t =
  match t.kind with
  | Lost -> -1.
  | _ -> ucb sum_visits t.p t.w t.n;;

let reward ((p, _), _) tree =
  match tree.kind with
  | Won -> 1. | Lost -> 0.
  | _ ->
     if !predict_value then begin
       let logistic v =  1. /. (1. +. exp (0. -. v)) in
       let f = fea_global_update p; get_fea p in
       totfea := !totfea + List.length f;
       logistic (Xgb.predict_v f)
     end else !value_factor;;

let start_time = Sys.time ();;

exception ResourceOut of string;;

let cleanexit _ = raise (ResourceOut "Signal");;
Sys.signal Sys.sigint (Sys.Signal_handle cleanexit);;
Sys.signal Sys.sigterm (Sys.Signal_handle cleanexit);;

let check_limits () =
  if !infer >= !max_infs then raise (ResourceOut "Infer");
  if Sys.time () -. start_time >= !max_time then raise (ResourceOut "Time");
  if Xgb.c_mem () >= !max_mem then raise (ResourceOut "Mem")
;;

let rec playout depth (st, tree, thist, acts) =
  check_limits ();
  if tree.kind = Open && depth >= 0 then
    let i = arg_max (get_rel tree.n) tree.b in
    playout (depth - 1) (do_tree (List.nth tree.b i) thist (infer := !infer + 1; Ff.extend st (List.nth acts i)))
  else begin
    if tree.kind = Unexplored then tree.kind <- Open;
    tree.r <- reward st tree;
    let update_tree win t = t.w <- t.w +. win; t.n <- t.n + 1 in
    List.iter (update_tree tree.r) thist
  end;;

let logit x =
  if x = 1. then 10. else if x = 0. then -10. else
  let ret = log (x /. (1. -. x)) in
  if ret < -10. then -10. else if ret > 10. then 10. else ret;;

let print_guides init_tree won =
  let do_seq oc ps = List.iter (fun (i,n) -> Printf.fprintf oc " %d:%d" i n) ps in
  let ((((p, _), _) as st), t, _, al) = init_tree in
  let ts = if won then !is_theorem else !bigstep_trees in
  Ff.restart st;
  let ocv = open_out (Sys.argv.(2) ^ "/" ^ Sys.argv.(3) ^ ".v") in
  let ocp = open_out (Sys.argv.(2) ^ "/" ^ Sys.argv.(3) ^ ".p") in
  let l = List.length ts - 1 in
  let rec print_more p t dist st al =
    if dist = 0 then () else
    if t.kind <> Open then failwith ("aaa" ^ string_of_int dist) else
    let f = fea_global_update p; get_fea p in
    Printf.fprintf ocv "%.5f" (logit (if won then (0.98 ** float_of_int dist) else 0.)); do_seq ocv f; Printf.fprintf ocv "\n";
    if won then begin
      let fealist = List.map (get_act_fea p) al in
      let norm_vsum = float t.n /. float (List.length t.b) in
      let maybe_print_p t f =
        if 0 < t.n then
          let p = log (float t.n /. norm_vsum) in
          let p = if p < -6. then -6. else p in
          (Printf.fprintf ocp "%.5f" p; do_seq ocp f; Printf.fprintf ocp "\n")
      in
      List.iter2 maybe_print_p t.b fealist
    end;
    let nt, na = List.find (fun (nt, na) -> List.memq nt ts) (List.combine t.b al) in
    let _, (ns, al) = extend1 st na in
    print_more p nt (dist - 1) ns al
  in
  print_more p t l st al;
  close_out ocv; close_out ocp
;;


exception Solved;;
exception DeadEnd;;

let bigstep_hist = ref [];;

let rec bigstep ((st, tree, thist, acts) as state) =
  if tree.kind = Unexplored then tree.kind <- Open; (* freshly visited *)
  let i = ref 0 in
  while !i < !play_count (*&& tree.n < 200*) do
    incr i; playout !play_dep state;
    check_limits ();
  done;
  if tree.kind = Won then raise Solved;
  if tree.kind = Lost then raise DeadEnd;
  let i =
    if !is_theorem = [] || !thm_play_count = -1 then
      arg_max (fun t -> if t.n = 0 then 0. else float t.n +. t.w /. float t.n) tree.b
    else
      arg_max (fun t -> if t.kind = Won then 2 else if List.memq t !is_theorem then 1 else 0) tree.b
  in
  incr bigsteps;
  (*if !bigsteps > 200 then raise DeadEnd;*)
  bigstep_hist := i :: !bigstep_hist;
  bigstep_trees := tree :: !bigstep_trees;
  bigstep (do_tree (List.nth tree.b i) thist (Ff.extend st (List.nth acts i)));;

at_exit (fun () ->
  printf "%% Proof: %s\n" (String.concat " " (List.map string_of_int (List.rev !bigstep_hist)));
  printf "%% Bigsteps: %i Inf: %i Op: %i Cl: %i Ed:%i TotFea:%i Tim:%f\n" !bigsteps !infer !opened !closed !edges !totfea (Sys.time () -. start_time);
);;

let tosolve = ref Sys.argv.(1);;
if !tosolve <> "" then
  let (_, (((p, _), _), _)) as init_state = Ff.start !tosolve in
  let () = Ff.fea_init p 0 in
  let init_tree = do_tree itree [] init_state in
  try
    playout !play_dep init_tree; bigstep init_tree
  with Solved -> printf "%% SZS status Theorem (fast)\n%!"; print_guides init_tree true; print_dot itree
     | Failure x -> printf "%% SZS status Error\n%%%s\n%!" x
     | DeadEnd -> printf "%% SZS status DeadEnd\n%!"; print_guides init_tree false
     | ResourceOut x when !is_theorem = [] -> printf "%% SZS status ResourceOut: %s\n%!" x; print_guides init_tree false
     | ResourceOut x -> printf "%% SZS status Theorem (slow)\n%%"; print_guides init_tree true
     | Parsing.Parse_error -> printf "%% SZS status Error\n%%Parse_error\n%!";;
