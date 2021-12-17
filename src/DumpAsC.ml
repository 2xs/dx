(**************************************************************************)
(*  This file is part of dx, a tool to derive C from monadic Gallina.     *)
(*                                                                        *)
(*  Copyright (C) 2021 Université de Lille & CNRS                         *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation; either version 2 of the License, or     *)
(*  (at your option) any later version.                                   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(**************************************************************************)

(* From stdlib *)
open Char
open Filename
open Format

(* From CompCert *)
open Camlcoq
open PrintCsyntax

(* From dx *)
open ResultMonad
open DXModule

let register_string (a,s) =
    let s = camlstring_of_coqstring s in
    match Hashtbl.find_opt string_of_atom a with
    | None -> let s = if Hashtbl.mem atom_of_string s
                      then s ^ "$" ^ Z.to_string (Z.Zpos a)
                      else s in
              assert (not (Hashtbl.mem atom_of_string s)) ;
              Hashtbl.add atom_of_string s a ;
              Hashtbl.add string_of_atom a s ;
              if P.ge a !next_atom then next_atom := P.succ a
    | Some s' ->
        (* allow to register the same string-atom correspondance
           multiple times *)
        assert (s == s')

let change_char c =
    if c >= 'a' && c <= 'z' then uppercase_ascii c
    else if (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c
    else '_'

let print_begin_guard f p =
    let b = Filename.basename p in
    let g = String.map change_char b in
    let h = "#ifndef " ^ g ^ "\n" in
    let h = h ^ "#define " ^ g ^ "\n\n" in
    Format.pp_print_as f 0 h

let print_end_guard f =
    Format.pp_print_as f 0 "#endif\n" ;
    Format.pp_print_flush f ()

(* print_dx : list (string * Result dxModule) -> unit *)
let print_dx_modules mods =
    let go (path, res_mod) =
        match res_mod with
        | Ok m ->
            ( assert (Hashtbl.length atom_of_string == 0) ;
              assert (Hashtbl.length string_of_atom == 0) ;
              let p = camlstring_of_coqstring path in
              let h = Filename.extension p = ".h" in
              let o = open_out p in
              let f = Format.formatter_of_out_channel o in
              List.iter register_string m.dxModuleNames ;
              if h then print_begin_guard f p ;
              print_program f m.dxModuleContent ;
              if h then print_end_guard f ;
              close_out o ;
              Hashtbl.reset atom_of_string ;
              Hashtbl.reset string_of_atom )
        | Err _ -> exit 1
    in
        List.iter go mods
