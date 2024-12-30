open Ll
open Datastructures

(* The lattice of symbolic constants ---------------------------------------- *)
module SymConst = struct
  type t =
    | NonConst (* Uid may take on multiple values at runtime *)
    | Const of int64 (* Uid will always evaluate to const i64 or i1 *)
    | UndefConst (* Uid is not defined at the point *)

  let compare (a : t) (b : t) =
    match a, b with
    | Const i, Const j -> Int64.compare i j
    | NonConst, NonConst | UndefConst, UndefConst -> 0
    | NonConst, _ | _, UndefConst -> 1
    | UndefConst, _ | _, NonConst -> -1
  ;;

  let to_string : t -> string = function
    | NonConst -> "NonConst"
    | Const i -> Printf.sprintf "Const (%LdL)" i
    | UndefConst -> "UndefConst"
  ;;
end

(* The analysis computes, at each program point, which UIDs in scope will evaluate 
   to integer constants *)
type fact = SymConst.t UidM.t

let eval_binop (bop)= 
  let open Int64 in
  match bop with
  | Add -> add
  | Sub -> sub
  | Mul -> mul
  | Shl -> (fun x y -> shift_left x (to_int y))
  | Lshr -> (fun x y -> shift_right_logical x (to_int y))
  | Ashr -> (fun x y -> shift_right x (to_int y))
  | And -> logand
  | Or -> logor
  | Xor -> logxor
;;

let eval_cnd (cnd : cnd) (i1 : int64) (i2 : int64) =
  let open Int64 in
  let cmp = 
    match cnd with
    | Eq -> equal i1 i2
    | Ne -> not @@ equal i1 i2
    | Slt -> (compare i1 i2) < 0
    | Sle -> (compare i1 i2) <= 0
    | Sgt -> (compare i1 i2) > 0
    | Sge -> (compare i1 i2) >= 0
  in
  if cmp then 1L else 0L
;;
(* flow function across Ll instructions ------------------------------------- *)
(* - Uid of a binop or icmp with const arguments is constant-out with
     result that is computed statically (see the Int64 module)
   - Uid of a binop or icmp with an UndefConst argument is UndefConst-out
   - Uid of a binop or icmp with an NonConst argument is NonConst-out
   - Uid of stores and void calls are UndefConst-out
   - Uid of all other instructions are NonConst-out
*)
let insn_flow ((uid, insn) : uid * insn) (fact : fact) : fact =
  let open SymConst in

  let get_value x = match UidM.find_opt x fact with
    | Some value -> value
    | None -> NonConst
  in

  let cmb f = function
    | (UndefConst, _) | (_, UndefConst) -> UndefConst
    | (NonConst, _) | (_, NonConst) -> NonConst
    | (Const val1, Const val2) -> Const (f val1 val2)
  in

  let evaluate_instruction = function
    | Binop (binop, ty, Const val1, Const val2) ->
      cmb (eval_binop binop) (Const val1, Const val2)
    | Binop (binop, ty, Id id1, Id id2) ->
      cmb (eval_binop binop) (get_value id1, get_value id2)
    | Binop (binop, ty, Const val1, Id id2) ->
      cmb (eval_binop binop) (Const val1, get_value id2)
    | Binop (binop, ty, Id id1, Const val2) ->
      cmb (eval_binop binop) (get_value id1, Const val2)

    | Icmp (cnd, ty, Const val1, Const val2) ->
      cmb (eval_cnd cnd) (Const val1, Const val2)
    | Icmp (cnd, ty, Id id1, Id id2) ->
      cmb (eval_cnd cnd) (get_value id1, get_value id2)
    | Icmp (cnd, ty, Const val1, Id id2) ->
      cmb (eval_cnd cnd) (Const val1, get_value id2)
    | Icmp (cnd, ty, Id id1, Const val2) ->
      cmb (eval_cnd cnd) (get_value id1, Const val2)
    | Store _ | Call (Void, _, _) -> UndefConst
    | _ -> NonConst
  in

  let result = evaluate_instruction insn in
  UidM.add uid result fact
(* The flow function across terminators is trivial: they never change const info *)
let terminator_flow (t : terminator) (d : fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact = struct
  type t = fact

  let forwards = true
  let insn_flow = insn_flow
  let terminator_flow = terminator_flow
  let normalize : fact -> fact = UidM.filter (fun _ v -> v != SymConst.UndefConst)

  let compare (d : fact) (e : fact) : int =
    UidM.compare SymConst.compare (normalize d) (normalize e)
  ;;

  let to_string : fact -> string = UidM.to_string (fun _ v -> SymConst.to_string v)

  (* The constprop analysis should take the meet over predecessors to compute the
       flow into a node. You may find the UidM.merge function useful *)
  let combine (ds : fact list) : fact =
    (* failwith "Constprop.Fact.combine unimplemented" *)
    let merge_values _ val1 val2 =
      match val1, val2 with
      | Some SymConst.NonConst, _ | _, Some SymConst.NonConst -> Some SymConst.NonConst
      | Some (SymConst.Const const1), Some (SymConst.Const const2) ->
        if const1 = const2 then Some (SymConst.Const const1) else Some SymConst.NonConst
      | Some SymConst.UndefConst, _ | _, Some SymConst.UndefConst ->
        Some SymConst.UndefConst
      | None, Some sym_const | Some sym_const, None -> Some sym_const
      | None, None -> None
    in
    List.fold_left (UidM.merge merge_values) UidM.empty ds
  ;;
end

(* instantiate the general framework ---------------------------------------- *)
module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* expose a top-level analysis operation ------------------------------------ *)
let analyze (g : Cfg.t) : Graph.t =
  (* the analysis starts with every node set to bottom (the map of every uid 
     in the function to UndefConst *)
  let init l = UidM.empty in
  (* the flow into the entry node should indicate that any parameter to the
     function is not a constant *)
  let cp_in =
    List.fold_right (fun (u, _) -> UidM.add u SymConst.NonConst) g.Cfg.args UidM.empty
  in
  let fg = Graph.of_cfg init cp_in g in
  Solver.solve fg
;;

(* run constant propagation on a cfg given analysis results ----------------- *)
(* HINT: your cp_block implementation will probably rely on several helper 
   functions.                                                                 *)
let run (cg : Graph.t) (cfg : Cfg.t) : Cfg.t =
  let open SymConst in
  let cp_block (l : Ll.lbl) (cfg : Cfg.t) : Cfg.t =
    let b = Cfg.block cfg l in
    let cb = Graph.uid_out cg l in
    (* failwith "Constprop.cp_block unimplemented" *)
    let b = Cfg.block cfg l in
    let cb = Graph.uid_out cg l in
    let aux_instr ((id,ins) :(uid * insn)) : (uid * insn) =
      let temp = cb id in
      let auxx (op: operand) : operand =
        begin match op with 
          | Gid id | Id id -> begin match UidM.find_opt id temp with
              | Some (Const x) -> Const x
              | _ -> op
            end
          | _ -> op
        end in
      begin match ins with 
        | Binop (bop, ty, op1, op2) -> (id, Binop (bop, ty, auxx op1, auxx op2))
        | Icmp (cnd, ty, op1, op2) -> (id, Icmp (cnd, ty, auxx op1, auxx op2))
        | Load (ty, op) -> (id, Load (ty, auxx op))
        | Store (ty, op1, op2) -> (id, Store (ty, auxx op1, auxx op2))
        | Call (ty, op, op_list) -> (id, Call(ty, auxx op, List.map (fun (x,y) -> (x, auxx y)) op_list))
        | Bitcast (ty1, op, ty2) -> (id, Bitcast (ty1, auxx op, ty2))
        | _ -> (id, ins)
      end
    in
    let aux_term ((id,term) :(uid * terminator)) : (uid * terminator) =
      let temp = cb id in
      let auxx (op: operand) : operand =
        begin match op with 
          | Gid id | Id id -> begin match UidM.find id temp with
              | Const x -> Const x
              | _ -> op
            end
          | _ -> op
        end in
      begin match term with 
        | Ret (ty, (Some op)) -> (id, Ret (ty, (Some (auxx op))))
        | Cbr (op, l1, l2) -> (id, Cbr (auxx op,l1,l2))
        | _ -> (id, term)
      end
    in
    let block = {insns = List.map aux_instr b.insns; term = aux_term b.term} in
    {cfg with blocks=LblM.add l block (LblM.remove l cfg.blocks)}
  in
  LblS.fold cp_block (Cfg.nodes cfg) cfg
;;
