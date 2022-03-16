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

From Coq Require Import BinNums List Ascii String Nat ZArith.
From Coq Require Import Numbers.AltBinNotations.
Import List.ListNotations.

From compcert.cfrontend Require Csyntax Ctypes.
From compcert.common Require Errors.

From dx Require Import ResultMonad IR CoqIR IRtoC DXModule DumpAsC.
From dx.Type Require Bool Nat.

Import UserIdentNotations.

Open Scope string.

Definition state := nat.

Definition M (A: Type) := state -> option (A * state).

Definition runM {A: Type} (x: M A) (s: state) := x s.
Definition returnM {A: Type} (a: A) : M A := fun s => Some (a, s).
Definition emptyM {A: Type} : M A := fun s => None.
Definition bindM {A B: Type} (x: M A) (f: A -> M B) : M B :=
  fun s =>
    match runM x s with
    | None => None
    | Some (x', s') => runM (f x') s'
    end.

Definition get : M state := fun s => Some (s, s).
Definition put (s: state) : M unit := fun s' => Some (tt, s).

Declare Scope monad_scope.
Notation "'do' x <- a ; b" :=
  (bindM a (fun x => b))
    (at level 200, x name, a at level 100, b at level 200)
  : monad_scope.

Open Scope monad_scope.

Definition ready : M bool :=
  do s <- get ;
     returnM (even s).

Definition getReady : M unit :=
  do s <- get ;
     if even s
     then returnM tt
     else do _ <- put (S s) ;
             returnM tt.

Definition id (b : bool) : M bool := returnM b.

Definition neg (b : bool) : M bool := if b then returnM false else returnM true.

Definition emptyUnitM := @emptyM unit.

Fixpoint prepare recBound : M unit :=
  match recBound with
  | O   => emptyUnitM
  | S b => do r <- ready ;
              if r
              then returnM tt
              else do _ <- getReady ;
                      prepare b
  end.

Module ModTest.
Definition testId (b : bool) : M bool := returnM b.
End ModTest.

Definition boolToBoolType := MkCompilableSymbolType [Bool.boolCompilableType] (Some Bool.boolCompilableType).
Definition derivableId := MkDerivableSymbol M "id" true boolToBoolType id false.

Definition externEmptyUnitM := MkDerivableSymbol M "emptyUnitM" true (MkCompilableSymbolType [] None) emptyUnitM true.
Definition externReady := MkDerivableSymbol M "ready" true Bool.boolSymbolType ready true.
Definition externGetReady := MkDerivableSymbol M "getReady" true (MkCompilableSymbolType [] None) getReady true.

Definition derivableNeg := MkDerivableSymbol M "neg" true boolToBoolType neg false.

Axiom axiom : nat.

Record info := MkInfo
  { infoA: bool
  ; infoB: bool
  }.

Definition infoComposite :=
  Ctypes.Composite
    $"info_s"
    Ctypes.Struct
    [ Ctypes.Member_plain $"info_a" Ctypes.type_bool;
      Ctypes.Member_plain $"info_b" Ctypes.type_bool ]
    Ctypes.noattr.

Definition infoCType := Ctypes.Tstruct $"info_s" Ctypes.noattr.

Definition infoCompilableType := MkCompilableType info infoCType.
Definition infoToBoolSymbolType :=
  MkCompilableSymbolType [infoCompilableType] (Some Bool.boolCompilableType).

Definition infoProjA :=
  MkPrimitive infoToBoolSymbolType
              infoA
              (fun es => match es with
                         | [s] => Ok (Csyntax.Efield s $"info_a" Ctypes.type_bool)
                         | _   => Err PrimitiveEncodingFailed
                         end).

Definition getInfoA (i: info) : M bool :=
  returnM (infoA i).

Close Scope monad_scope.

(***************************************)

(* Activate the verbose attribute to trace GenerateIntermediateRepresentation *)
(* It can also be activated globally by setting the environment variable
     COQ_ELPI_ATTRIBUTES=verbose
 *)

(* #[verbose] *)
GenerateIntermediateRepresentation HeaderSymbolIRs
  M bindM returnM
  Bool.Exports
  Nat.Exports
  derivableId
  axiom
  infoCompilableType
  infoProjA
  neg
  ModTest
  externEmptyUnitM
  externReady
  externGetReady
  prepare
  getInfoA.

Definition dxModuleTestH := makeDXModuleWithUserIds [infoComposite] ["info_s";"info_a";"info_b"] HeaderSymbolIRs.

(* #[verbose] *)
GenerateIntermediateRepresentation SymbolIRs
  M bindM returnM
  Bool.Exports
  Nat.Exports
  derivableId
  axiom
  infoCompilableType
  infoProjA
  __
  neg
  ModTest
  externEmptyUnitM
  externReady
  externGetReady
  prepare
  getInfoA.

Definition dxModuleTest := makeDXModuleWithUserIds [infoComposite] ["info_s";"info_a";"info_b"] SymbolIRs.
