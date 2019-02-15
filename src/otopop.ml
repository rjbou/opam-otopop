(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Cmdliner
open OpamTypes

(* Add atoms in dependencies *)
let update_depends map_atoms depends =
  let new_depends, updated, added =
    let ands_dep = OpamFormula.ands_to_list depends in
    let new_depends, map_atoms, updated =
      List.fold_left (fun (deps, map_atoms, upd) form ->
          match form with
          | Atom (n, filter) as old_atom ->
            (match OpamPackage.Name.Map.find_opt n map_atoms with
             | Some filter' ->
               let updated =
                 if filter <> filter' then
                   (* remove all version constraints *)
                   let purged_filter =
                     OpamFormula.map_formula (function
                         | Atom (Constraint (_, FString _))-> Empty
                         | x -> x) filter
                   in
                   let new_filter =
                     match filter', purged_filter with
                     | Empty, _ -> purged_filter
                     | _, Empty -> filter'
                     | _,_ -> And (filter', purged_filter)
                   in
                   if filter <> new_filter then
                     let new_atom = Atom (n, new_filter) in
                     Some ((new_atom :: deps),
                           (OpamPackage.Name.Map.remove n map_atoms),
                           (old_atom, new_atom)::upd)
                   else None
                 else None
               in
               (match updated with
                | Some updated -> updated
                | None ->
                  (old_atom::deps),
                  (OpamPackage.Name.Map.remove n map_atoms),
                  upd)
             | None -> (old_atom::deps), map_atoms, upd)
          | r -> ((r::deps), map_atoms, upd))
        ([], map_atoms, []) ands_dep
    in
    if OpamPackage.Name.Map.is_empty map_atoms then
      new_depends, updated, []
    else
      let new_atoms =
        OpamPackage.Name.Map.bindings map_atoms
        |> List.map (fun (n,f) -> Atom (n, f))
      in
      new_atoms @ new_depends, updated, new_atoms
  in
  let new_depends = OpamFormula.ands (List.rev new_depends) in
  new_depends, updated, added

(* Add pinned packages *)
let update_pin_depends st atoms pin_depends =
  let open OpamStd.Option.Op in
  OpamStd.List.filter_map (fun n ->
      let nv =
        (if OpamSwitchState.is_pinned st n then
           Some n else None) >>=
        (fun n ->
           try Some (OpamPackage.package_of_name st.installed n)
           with Not_found -> None)
      in
      let u =
        nv
        >>= OpamSwitchState.opam_opt st
        >>= OpamFile.OPAM.get_url
      in
      match nv, u with
      | Some nv, Some u ->
        (match OpamStd.List.find_opt (fun (nv',_) ->
             nv.name = nv'.name) pin_depends with
         | Some (nv', u') ->
           if u <> u' &&
              OpamConsole.confirm ~default:false
                "Replace pin_depends [%s %s] by [%s %s]?"
                (OpamPackage.to_string nv')
                (OpamUrl.to_string u')
                (OpamPackage.to_string nv)
                (OpamUrl.to_string u) then
             Some (nv,u)
           else None
         | None -> Some (nv,u))
      | _, _ -> None)
    atoms

let opam_files_in_dir d =
  List.map (fun (_,f) -> OpamFile.filename f)
    (OpamPinned.files_in_source d)

let check_opam_file output =
  let strf f = OpamConsole.colorise `underline (OpamFilename.to_string f) in
  let get_from_dir d =
    (match opam_files_in_dir d with
     | [file] -> file
     | [] ->
       OpamConsole.error_and_exit `Not_found
         "No opam file in %s" (OpamFilename.Dir.to_string d)
     | _ ->
       OpamConsole.error_and_exit `Not_found
         "Multiple opam files in %s" (OpamFilename.Dir.to_string d))
  in
  let file =
    match output with
    | Some (OpamFilename.D d) ->
      let f = get_from_dir d in
      OpamConsole.msg "Updating file %s\n" (strf f);
      f
    | Some (OpamFilename.F f) -> f
    | None ->
      let f = get_from_dir (OpamFilename.Dir.of_string ".") in
      if OpamConsole.confirm "Update local opam file %s?" (strf f) then
        f
      else
        OpamStd.Sys.exit_because `Aborted
  in
  (* test that there is no parsing error *)
  let warnerr, _ = OpamFileTools.lint_file (OpamFile.make file) in
  let warn, err =
    List.fold_left (fun (w,e) (n,we,_s) ->
        match we with
        | `Warning -> n::w, e
        | `Error -> w, n::e) ([],[]) warnerr
  in
  if err <> [] then
    OpamConsole.error_and_exit `File_error "Linting error%s %s found in %s."
      (match err with | [_] -> "" | _ -> "s")
      (OpamStd.List.to_string string_of_int err |> OpamConsole.colorise `bold)
      (strf file)
  else if warn <> [] then
    OpamConsole.warning "Linting warning%s %s found in %s."
      (match warn with | [_] -> "" | _ -> "s")
      (OpamStd.List.to_string string_of_int warn |> OpamConsole.colorise `bold)
      (strf file)

let get_opam_file output =
  let get_from_dir d =
    match opam_files_in_dir d with
    | [file] -> file
    | _ -> assert false
  in
  match output with
  | Some (OpamFilename.D d) -> get_from_dir d
  | Some (OpamFilename.F f) -> f
  | None -> get_from_dir (OpamFilename.Dir.of_string ".")

let auto_populate output atoms_or_locals depopt ~check =
  let file = get_opam_file output in
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamSwitchState.with_ `Lock_none gt @@ fun st ->
  let map_atoms, dropped_atoms =
    let aggregated_formulas =
      List.fold_left (fun map -> function
          | `Atom (n,vc) ->
            let vcs =
              match OpamPackage.Name.Map.find_opt n map with
              | Some vcs -> vcs
              | None -> []
            in
            OpamPackage.Name.Map.add n (vc::vcs) map
          | _ -> map)
        OpamPackage.Name.Map.empty atoms_or_locals
    in
    OpamPackage.Name.Map.fold (fun n vcs (map, dropped) ->
        let installed =
          try
            let pkg = OpamSwitchState.find_installed_package_by_name st n in
            List.for_all (fun vc -> OpamFormula.check (n,vc) pkg) vcs
          with Not_found -> false
        in
        let vform =
          OpamFormula.ands (List.map (function
              | None -> OpamFormula.Empty
              | Some (op,v) ->
                OpamFormula.Atom
                  (Constraint
                     (op, FString (OpamPackage.Version.to_string v))))
              vcs)
        in
        if installed then
          (OpamPackage.Name.Map.add n vform map), dropped
        else
          map, n::dropped)
      aggregated_formulas (OpamPackage.Name.Map.empty, [])
  in
  if check && not (OpamPackage.Name.Map.is_empty map_atoms || dropped_atoms <> []) then
    OpamConsole.error_and_exit `Internal_error
      "inconsistent state: install failed, but no package is found missing...";
  (* check what has been installed because of install fail *)
  if OpamPackage.Name.Map.is_empty map_atoms then
    OpamConsole.msg "No package has been installed, nothing to do.\n"
  else
    (if dropped_atoms <> [] then
       OpamConsole.warning "Packages %s not installed, skip %s."
         (OpamStd.List.concat_map ", "  ~last_sep:"and "
            (fun n -> OpamConsole.colorise `bold (OpamPackage.Name.to_string n))
            dropped_atoms)
         (if List.length dropped_atoms = 1 then "it" else "them");
     let opam0 = OpamFile.make file in
     let opam = OpamFile.OPAM.read opam0 in
     let new_depends, dep_updated, dep_added =
       update_depends map_atoms
         (OpamFile.OPAM.(if depopt then depopts else depends) opam)
     in
     let new_pin_depends, pindep_updated =
       let pin_depends = (OpamFile.OPAM.pin_depends opam) in
       let new_pins =
         update_pin_depends st (OpamPackage.Name.Map.keys map_atoms) pin_depends
       in
       List.fold_left (fun new_pin (nv',u') ->
           OpamStd.List.update_assoc nv' u' new_pin) pin_depends new_pins, new_pins
     in
     if dep_updated = [] && dep_added = [] && pindep_updated = [] then
       OpamConsole.msg "Everything is up-to-date.\n"
     else
       (OpamFile.OPAM.(if depopt then with_depopts else with_depends) new_depends opam
        |> OpamFile.OPAM.with_pin_depends new_pin_depends
        |> OpamFile.OPAM.write_with_preserved_format ~format_from:opam0 opam0;
        let to_print =
          let str f l =
            match l with
            | [x] -> f x
            | _ -> "\n" ^ (OpamStd.Format.itemize f l)
          in
          (if dep_updated = [] then [] else
             [ "update",
               (str (fun (old_atom, new_atom) ->
                    Printf.sprintf "[ %s ] -> [ %s ]"
                      (OpamFilter.string_of_filtered_formula old_atom)
                      (OpamFilter.string_of_filtered_formula new_atom))
                   dep_updated)
             ])
          @
          (if dep_added = [] then [] else
             [ "add",
               (str OpamFilter.string_of_filtered_formula dep_added) ])
          @
          if pindep_updated = [] then [] else
            [ "pin_depends",
              (str (fun (nv,u) ->
                   Printf.sprintf "%s %s" (OpamPackage.to_string nv)
                     (OpamUrl.to_string u))
                  pindep_updated)
            ]
        in
        OpamConsole.msg "Update %sdependencies in %s:\n%s"
          (if depopt then "optionnal " else "")
          (OpamFilename.to_string file)
          (OpamStd.Format.itemize (fun (label, str) -> label ^ ": " ^ str) to_print)))

(* main *)
let doc = "Install package and automatically populate opam file"
let man = [
  `S "DESCRIPTION";
  `P "Install $(i,PACKAGES) and automatically populate the given \
      $(i,opam-file): add or update the successfully installed packages in the \
      `depends:` field (or `depopts:` if specified), add/update \
      `pin-depends:` field.";
  `S "OPTIONS";
  `S OpamArg.build_option_section;
]

let output =
  OpamArg.mk_opt ["opam-file"] "FILE"
    "$(b,[otopop]) Name of the opam file to update, or directory containing it. \
     Current directory if unspecified. Raises an error if more thant one opam \
     files is found."
    OpamArg.existing_filename_dirname_or_dash None

let depopt =
  OpamArg.mk_flag ["depopt"] "$(b,[otopop]) Add dependency in $(i,depopts) field"

(* We use a ref here to be able to catch error from install command *)
let auto_pop_fun = ref (fun ~check:_ -> ())

let pre_checking global_options output atomlocs depopt =
  OpamArg.apply_global_options global_options;
  check_opam_file output;
  auto_pop_fun := auto_populate output atomlocs depopt;
  fun () -> ()

let install_otopop =
  Term.((const pre_checking) $ OpamArg.global_options $ output
        $ OpamArg.atom_or_local_list $ depopt
        $ (fst OpamCommands.install)
       ), Term.info "opam-otopop" ~man ~doc

let () =
  let buff = Buffer.create 0124 in
  let fmt = Format.formatter_of_buffer buff in
  let check =
    match Term.eval ~err:fmt ~catch:true install_otopop with
    | `Error `Exn ->
      Format.pp_print_flush fmt ();
      OpamConsole.msg "\n";
      OpamConsole.error "opam install ended with a %s exit code:%s\n"
        (OpamConsole.colorise `bold "non-zero")
        (let err = Buffer.contents buff in
         let err_code =
           OpamStd.Option.Op.(
             OpamStd.String.cut_at err '!'
             >>| snd
             >>= fun s -> OpamStd.String.rcut_at s ')')
         in
         match err_code with
         | Some (c,_) -> Printf.sprintf " %s." (OpamConsole.colorise `bold c)
         | None -> "\n" ^ (OpamStd.String.remove_prefix ~prefix:"opam-otopop: " err));
      true
    | `Error (`Term | `Parse) ->
      Format.pp_print_flush fmt ();
      OpamConsole.error_and_exit `Bad_arguments "%s" (Buffer.contents buff)
    | _ -> false
  in
  !auto_pop_fun ~check
