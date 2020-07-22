(* TODO Investigate untagged integers *)
type problem;;
type prover;;

external mk_problem : (int * int list) list * (int * int list) list * (int * string) list * (int * int * int list) list -> problem = "ff_problem";;
external print_problem : problem -> unit = "ff_print_problem";;
external print_prover : prover -> unit = "ff_print_prover";;

type action = ALem of int | ARed of int | AExt of int;;
let a2i = function ALem i -> 100000 + i | ARed i -> 200000 + i | AExt i -> 300000 + i;;
let a2s = function AExt i -> "E" ^ string_of_int i | ARed i -> "R" ^ string_of_int i | ALem i -> "L" ^ string_of_int i;;

external mk_prover : problem -> prover = "ff_prover";;
external get_acs : prover -> action list = "ff_get_acs";;
external action : prover -> action -> bool = "ff_action";;
(*let action p a = Format.printf "<%i>%!" (a2i a); action p a;;*)
external action_index : prover -> int -> bool = "ff_action_index";;
external save : prover -> int = "ff_save";;
external restore : prover -> int -> unit = "ff_restore";;
external query : prover -> int list -> int * int = "ff_query";;

external fea_init : prover -> int -> unit = "ff_fea_init";;
external fea_global_update : prover -> unit = "ff_fea_global_update";;
external get_fea : prover -> (int * int) list = "ff_get_fea";;
external get_act_fea : prover -> action -> (int * int) list = "ff_get_act_fea";;


let state p proved =
  let bp = save p in
  if proved then (1, ((p, bp), [])) else
  let acs = get_acs p in
  if acs = [] then (-1, ((p, bp), acs))
  else (0, ((p, bp), acs))
;;

let infer = ref 0;;

let extend0 (p, bp) i =
  incr infer;
  restore p bp;
(*  assert(let _acs = get_acs p in List.mem i _acs);*)
  let proved = action p i in
  state p proved
;;

let start0 fname =
  let p = mk_prover (mk_problem (Fof.file_mat true (-1) false 0 fname)) in
  state p false
;;

(*
act*
    (win * ((prover * bp) * alist))
     win * (((prover * bp) * (hists)) * alist)
 *)

(* hist is the (never empty) history saved forward, fwhist is reversed further positions *)
(* It stores pairs (action, output of state) *)
let rec extend1 (((p, bp), (hist, fwhist)) as st) act =
  let lift (win, (pbp, acs)) = (win, ((pbp, (hist, fwhist)), acs)) in
  let (_, (_, ((_, bp2), _))) as tp = List.hd !hist in
  if bp2 != bp then begin (* User implicitly jumped back *)
    fwhist := tp :: !fwhist; hist := List.tl !hist; if !hist = [] then raise Exit; extend1 st act
  end else
    match !fwhist with
    | (act2, (_ as tp)) :: t -> (* We have jumped back and have some options to go forward *)
       if act2 = act then (hist := (act2, tp) :: !hist; fwhist := List.tl !fwhist; lift tp)
       else (fwhist := []; extend1 st act)
    | [] ->
       let ret = extend0 (p, bp) act in hist := (act, ret) :: !hist; lift ret
;;

let start1 fname =
  let ((win, (pbp, acs)) as ret) = start0 fname in
  let hist = ref [(ALem(-1), ret)] and fwhist = ref [] in
  (win, ((pbp, (hist, fwhist)), acs));;

let extend = extend1;;
let start = start1;;
let rec restart (((p, bp), (hist, fwhist)) as st) =
  let (_, (_, ((_, bp2), _))) as tp = List.hd !hist in
  if bp2 != bp then begin
    fwhist := tp :: !fwhist; hist := List.tl !hist; if !hist = [] then raise Exit; restart st
  end else begin
    restore p bp; fwhist := []
  end;;

