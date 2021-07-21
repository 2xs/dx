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

Require Import List.
Import ListNotations.

From compcert.cfrontend Require Csyntax Ctypes Cop.
From compcert.common Require Values.

From dx Require Import ResultMonad IR.

Definition boolCompilableType :=
  MkCompilableType bool Ctypes.type_bool.

Definition boolSymbolType :=
  MkCompilableSymbolType nil (Some boolCompilableType).

Definition boolMatchableType :=
  MkMatchableType boolCompilableType
    (fun cnd brchs =>
      match brchs with
      | [brchThen;brchElse] => Ok (Csyntax.Sifthenelse cnd brchThen brchElse)
      | _                   => Err MatchEncodingFailed
      end)
    [[];[]]
    [[];[]]
    (fun m A b whenTrue whenFalse => if b then whenTrue else whenFalse)
.

Definition boolFalse :=
  MkPrimitive boolSymbolType
              false
              (fun _ => Ok (Csyntax.Eval Values.Vfalse Ctypes.type_bool)).
Definition boolTrue  :=
  MkPrimitive boolSymbolType
              true
              (fun _ => Ok (Csyntax.Eval Values.Vtrue Ctypes.type_bool)).

Definition boolToBoolSymbolType :=
  MkCompilableSymbolType [boolCompilableType] (Some boolCompilableType).

Definition boolNeg :=
  MkPrimitive boolToBoolSymbolType
              negb
              (fun es => match es with
                         | (e :: nil) => Ok (Csyntax.Eunop Cop.Onotbool e Ctypes.type_bool)
                         | _          => Err PrimitiveEncodingFailed
                         end).

(* TODO: Add Eq, etc. *)

Module Exports.
  Definition boolMatchableType := boolMatchableType.
  Definition boolFalse := boolFalse.
  Definition boolTrue := boolTrue.
  Definition boolNeg := boolNeg.
End Exports.