(* Copyright (C) 2013, Christopher A. Wood
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor GVNPRE (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S

open Exp Transfer

fun transform (Program.T {globals, datatypes, functions, main}) =
    let 
    	(* Keep track of control-flow specific cse's,
       * arguments, and in-degree of blocks.
       *)
      val {get = labelInfo: Label.t -> {add: (Var.t * Exp.t) list ref,
                                        args: (Var.t * Type.t) vector,
                                        inDeg: int ref},
           set = setLabelInfo, ...} =
         Property.getSetOnce (Label.plist,
                              Property.initRaise ("info", Label.layout))


      (* Keep track of variables used as overflow variables. *)
      val {get = overflowVar: Var.t -> bool, set = setOverflowVar, ...} =
         Property.getSetOnce (Var.plist, Property.initConst false)


      (* Keep track of the replacements of variables. *)
      val {get = replace: Var.t -> Var.t option, set = setReplace, ...} =
         Property.getSetOnce (Var.plist, Property.initConst NONE)


      (* Keep track of the variable that holds the length of arrays (and
       * vectors and strings).
       *)
      val {get = getLength: Var.t -> Var.t option, set = setLength, ...} =
         Property.getSetOnce (Var.plist, Property.initConst NONE)


      fun canonVar x =
         case replace x of
            NONE => x
          | SOME y => y
      fun canonVars xs = Vector.map (xs, canonVar)


      (* Canonicalize an Exp.
       * Replace vars with their replacements.
       * Put commutative arguments in canonical order.
       *)
      fun canon (e: Exp.t): Exp.t =
         case e of
            ConApp {con, args} =>
               ConApp {con = con, args = canonVars args}
          | Const _ => e
          | PrimApp {prim, targs, args} =>
               let
                  fun doit args =
                     PrimApp {prim = prim,
                              targs = targs,
                              args = args}
                  val args = canonVars args
                  fun arg i = Vector.sub (args, i)
                  fun canon2 () =
                     let
                        val a0 = arg 0
                        val a1 = arg 1
                     in
                        (* What we really want is a total orderning on
                         * variables.  Since we don't have one, we just use
                         * the total ordering on hashes, which means that
                         * we may miss a few cse's but we won't be wrong.
                         *)
                        if Var.hash a0 <= Var.hash a1
                           then (a0, a1)
                        else (a1, a0)
                     end
                  datatype z = datatype Prim.Name.t
               in
                  if Prim.isCommutative prim
                     then doit (Vector.new2 (canon2 ()))
                  else
                     if (case Prim.name prim of
                            IntInf_add => true
                          | IntInf_andb => true
                          | IntInf_gcd => true
                          | IntInf_mul => true
                          | IntInf_orb => true
                          | IntInf_xorb => true
                          | _ => false)
                        then
                           let
                              val (a0, a1) = canon2 ()
                           in doit (Vector.new3 (a0, a1, arg 2))
                           end
                     else doit args
               end
          | Select {tuple, offset} => Select {tuple = canonVar tuple,
                                              offset = offset}
          | Tuple xs => Tuple (canonVars xs)
          | Var x => Var (canonVar x)
          | _ => e

		fun diag s =
	           Control.diagnostics
	           (fn display =>
	            let open Layout
	            in
	              display (seq [str ": ", str s])
	            end)
	    val _ = diag "COMPILED"

    (* Stub of function that walks the code blocks *)
    fun doitTree tree =
        let
          val blocks = ref []
        in
          blocks
        end

    val functions =
         List.revMap
         (functions, fn f =>
          let
            (* Not sure... *)
            val {args, blocks, mayInline, name, raises, returns, start} =
                Function.dest f

            (* Invoke the GVNPRE pass on each block of a function *)
            val blocks = walkBlocks (Function.dominatorTree f)
          in
            (* Return a new function with the optimizations applied *)
            Function.new {args = args,
                           blocks = blocks,
                           mayInline = mayInline,
                           name = name,
                           raises = raises,
                           returns = returns,
                           start = start}
          end)

		val program =
		 Program.T {datatypes = datatypes,
		            globals = globals,
		            functions = functions,
		            main = main}
    in
    	(* print "TODO" *)
    	program
    end 
end
