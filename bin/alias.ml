(** Alias Analysis *)

open Ll
open Datastructures

(* The lattice of abstract pointers ----------------------------------------- *)
module SymPtr =
  struct
    type t = MayAlias           (* uid names a pointer that may be aliased *)
           | Unique             (* uid is the unique name for a pointer *)
           | UndefAlias         (* uid is not in scope or not a pointer *)

    let compare : t -> t -> int = Stdlib.compare

    let to_string = function
      | MayAlias -> "MayAlias"
      | Unique -> "Unique"
      | UndefAlias -> "UndefAlias"

    let union sym1 sym2 = 
      begin match sym1, sym2 with
      | UndefAlias , s 
      | s , UndefAlias -> s
      | Unique , Unique -> Unique
      | MayAlias , _ 
      | _ , MayAlias -> MayAlias
      end
  end

(* The analysis computes, at each program point, which UIDs in scope are a unique name
   for a stack slot and which may have aliases *)
type fact = SymPtr.t UidM.t

(* flow function across Ll instructions ------------------------------------- *)
(* TASK: complete the flow function for alias analysis. 

   - After an alloca, the defined UID is the unique name for a stack slot
   - A pointer returned by a load, call, bitcast, or GEP may be aliased
   - A pointer passed as an argument to a call, bitcast, GEP, or store
     (as the value being stored) may be aliased
   - Other instructions do not define pointers

 *)

let insn_flow ((u,i):uid * insn) (d:fact) : fact =
  let update_d u sym = fun d ->
    (*UidM.update_or SymPtr.UndefAlias 
    (fun x -> SymPtr.union sym x) u d*)
    UidM.add u sym d
  in 
  begin match i with
  | Alloca _ -> 
      d |> update_d u SymPtr.Unique
  | Load (Ptr(Ptr _), _) -> 
      d |> update_d u SymPtr.MayAlias 
  | Bitcast (_, op, _) -> 
      let d' = match op with
        | Id ptr -> d |> update_d ptr SymPtr.MayAlias
        | _ ->      d
      in
      d' |> update_d u SymPtr.MayAlias
  | Store (Ptr _, Id ptr, _) ->  
      d |> update_d ptr SymPtr.MayAlias
  | Call (ty, _, args) -> 
      let d' = match ty with
        | Ptr _ -> d |> update_d u SymPtr.MayAlias
        | _ ->     d
      in
      List.fold_left 
        (fun syms (ty, op) -> match ty, op with
            | Ptr _ , Id ptr -> d |> update_d ptr SymPtr.MayAlias
            | _ ->              d
        ) d' args
  | Gep (_, op, _) ->
      let d' = match op with
        | Id ptr -> d |> update_d ptr SymPtr.MayAlias
        | _ ->      d
      in
      d' |> update_d u SymPtr.MayAlias
  | _ -> d
  end



(* The flow function across terminators is trivial: they never change alias info *)
let terminator_flow t (d:fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact =
  struct
    type t = fact
    let forwards = true

    let insn_flow = insn_flow
    let terminator_flow = terminator_flow
    
    (* UndefAlias is logically the same as not having a mapping in the fact. To
       compare dataflow facts, we first remove all of these *)
    let normalize : fact -> fact = 
      UidM.filter (fun _ v -> v != SymPtr.UndefAlias)

    let compare (d:fact) (e:fact) : int = 
      UidM.compare SymPtr.compare (normalize d) (normalize e)

    let to_string : fact -> string =
      UidM.to_string (fun _ v -> SymPtr.to_string v)

    (* TASK: complete the "combine" operation for alias analysis.

       The alias analysis should take the meet over predecessors to compute the
       flow into a node. You may find the UidM.merge function useful.

       It may be useful to define a helper function that knows how to take the
       meet of two SymPtr.t facts.
    *)
    let combine (ds:fact list) : fact =
      List.fold_left 
        (fun m1 m2 -> 
            UidM.union 
            (fun _ sym1 sym2 -> Some (SymPtr.union sym1 sym2)) 
            m1 m2 
        ) UidM.empty ds
  end

(* instantiate the general framework ---------------------------------------- *)
module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* expose a top-level analysis operation ------------------------------------ *)
let analyze (g:Cfg.t) : Graph.t =
  (* the analysis starts with every node set to bottom (the map of every uid 
     in the function to UndefAlias *)
  let init l = UidM.empty in

  (* the flow into the entry node should indicate that any pointer parameter 
     to the function may be aliased *)
  let alias_in = 
    List.fold_right 
      (fun (u,t) -> match t with
                    | Ptr _ -> UidM.add u SymPtr.MayAlias
                    | _ -> fun m -> m) 
      g.Cfg.args UidM.empty 
  in
  let fg = Graph.of_cfg init alias_in g in
  Solver.solve fg

