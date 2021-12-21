open Gospel

module Mutability = struct
  let max m n =
    let open Translated in
    match (m, n) with
    | Mutable, _ | _, Mutable -> Mutable
    | Dependant f, _ | _, Dependant f -> Dependant f
    | Unknown, _ | _, Unknown -> Unknown
    | Immutable, Immutable -> Immutable

  let min_mut = Translated.Immutable

  let tysymbol ~driver (ts : Ttypes.tysymbol) =
    (* To determine the mutability of a `tysymbol`, we look in the driver. *)
    match Drv.get_type ts driver with
    | None ->
        (* If the driver doesn't know about this `tysymbol`, the mutability is `Unknown`.
           Note that of the type is parametric we wait for the instantiation to deliver the
           information *)
        if List.length ts.ts_args > 0 then
          Translated.Dependant (fun _ -> Unknown)
        else Translated.Unknown
    | Some t -> t.mutable_

  let alpha (ty : Ttypes.ty) =
    match ty.ty_node with Tyvar _ -> true | _ -> false

  let rec ty ~driver (t : Ttypes.ty) =
    match t.ty_node with
    | Tyvar _ ->
        (* A `Tyvar` is an alpha *)
        Translated.Unknown
    | Tyapp (ts, tyl) when Ttypes.is_ts_tuple ts ->
        (* XXX not sure about that decision... *)
        (* In the presence of a tuple there are three cases
           1. There is at least an alpha in its element but another element is already known to be mutable
           2. There is at least an alpha but none element is known to be mutable
           3. All the elements are known types *)
        if List.exists alpha tyl then
          max
            (List.map (ty ~driver) tyl |> List.fold_left max min_mut)
            (Translated.Dependant (fun tyl -> List.fold_right max tyl min_mut))
        else List.map (ty ~driver) tyl |> List.fold_left max min_mut
    | Tyapp (ts, tyl) when List.length tyl = 0 ->
        (* If `tyl` is empty, we just look at `ts`. The mutability can't be `Dependant` *)
        tysymbol ~driver ts
    | Tyapp (ts, tyl) -> (
        (* It the list is not empty, that means that `ts` is a parametric type and
           its mutability is `Dependant` *)
        match tysymbol ~driver ts with
        | Translated.Dependant f as dep ->
            (* It there is still an alpha in the parameters, the mutability is still `Dependant`,
               otherwise, we apply the function embebed in the `Dependant` to the parameters *)
            if List.exists alpha tyl then dep else f (List.map (ty ~driver) tyl)
        | _ -> assert false)

  let lsymbol ~driver (ls : Tterm.lsymbol) =
    (* To determine the mutability of a `lsymbol` we look at its `ls_value`
       which is a `Ttypes.ty option`.
       If there is none, the mutability is unknown *)
    Option.fold ~none:Translated.Unknown ~some:(ty ~driver) ls.ls_value

  let constructor_declaration ~driver (cd : Tast.constructor_decl) =
    (* The mutability of a constructor is the max od the mutability of its argument *)
    List.map (ty ~driver) cd.cd_cs.ls_args |> List.fold_left max min_mut

  let field_declaration ~driver (ld : Tterm.lsymbol Tast.label_declaration) =
    (* A record field is mutable if it is annotated as mutable, if not we look
       at the lsymbol it contains i.e. the field itself. *)
    match ld.ld_mut with
    | Tast.Mutable -> Translated.Mutable
    | Tast.Immutable -> lsymbol ~driver ld.ld_field

  let type_declaration ~driver (td : Tast.type_declaration) =
    (* To determine the mutability of a type declaration, we check
       whether it is an alias. *)
    match td.td_ts.ts_alias with
    | Some alias ->
        (* An alias it a Ttypes.ty, e.g. `type t = int` *)
        ty ~driver alias
    | None -> (
        match td.td_kind with
        (* We don't have any information on an abstract type *)
        | Pty_abstract -> Translated.Unknown
        (* The mutability of a variant is the max of the mutability of its contructor *)
        | Pty_variant cdl ->
            List.map (constructor_declaration ~driver) cdl
            |> List.fold_left max min_mut
        | Pty_record rd ->
            (* The mutability of a record is the max of the mutability of its fields *)
            List.map (field_declaration ~driver) rd.rd_ldl
            |> List.fold_left max min_mut
        | Pty_open -> Translated.Unknown (* ? *))

  let mutable_model ~driver (ty_fields : (Tterm.lsymbol * bool) list) =
    List.map
      (* if a model is annotated as mutable, it is mutable, if not we look at
         the type of the model *)
        (fun (ls, b) -> if b then Translated.Mutable else lsymbol ~driver ls)
      ty_fields
    (* the mutability of a type is here the max of the mutability of its models *)
    |> List.fold_left max min_mut

  let type_spec ~driver (spec : Tast.type_spec) =
    (* To determine the mutability of a type according to its specification
       we check whether it is annotated with an ephemeral, and if not we
       look at its models *)
    if spec.ty_ephemeral then Translated.Mutable
    else mutable_model ~driver spec.ty_fields
end

module Equality = struct
  open Ppxlib

  let rec derive (t : Translated.type_) =
    let loc = t.loc in
    let polymorphic = [%expr fun a b -> a = b] in
    match t.name with
    | "unit" | "int" | "integer" | "char" | "string" | "bool" | "float" ->
        Some polymorphic
    | "option" -> (
        match derive (List.hd t.args) with
        | None -> None
        | Some eq ->
            Some
              [%expr
                fun a b ->
                  match (a, b) with
                  | None, None -> true
                  | Some a', Some b' -> [%e eq] a' b'
                  | _, _ -> false])
    | "list" -> (
        match derive (List.hd t.args) with
        | None -> None
        | Some eq -> Some [%expr fun a b -> List.for_all2 [%e eq] a b])
    | "array" -> (
        match derive (List.hd t.args) with
        | None -> None
        | Some eq -> Some [%expr fun a b -> Array.for_all2 [%e eq] a b])
    | "bag" -> (
        match derive (List.hd t.args) with
        | None -> None
        | Some eq ->
            (* XXX completely inefficient
               but we need comparison to sort the arrays and compare them pointwise with eq *)
            Some
              [%expr
                let occ a x =
                  Array.fold_right
                    (fun y n -> if [%e eq] x y then succ n else n)
                    a 0
                in
                fun a b ->
                  Array.for_all (fun x -> occ a x = occ b x) a
                  && Array.for_all (fun x -> occ b x = occ a x) b])
    | "set" -> (
        match derive (List.hd t.args) with
        | None -> None
        | Some eq ->
            Some
              [%expr
                fun a b ->
                  Array.for_all
                    (fun x -> Array.exists (fun i -> [%e eq] i x) b)
                    a
                  && Array.for_all
                       (fun x -> Array.exists (fun i -> [%e eq] i x) a)
                       b])
    | _ -> None
end
