(* Copyright (C) 2013, Christopher A. Wood
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor GVNPRE (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S

fun transform (Program.T {globals, datatypes, functions, main}) =
(*
   let
      val a = 1
   in
      program
   end
*)
end
