(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Random: RANDOM =
struct

local
   open MLton.Random
in
   val alphaNumString = alphaNumString
   val seed = seed
   val useed = useed
   val word = rand
   val srand = srand
end

val word = Trace.trace ("Random.word", Unit.layout, Word.layout) word
   
local
   val ri: int ref = ref 0
   val rw = ref (word ())
   val max = Word.wordSize - 1
in
   fun bool () =
      let
	 val i = !ri
	 val b = 0w1 = Word.andb (0wx1, Word.>> (!rw, Word.fromInt i))
	 val _ =
	    if i = max
	       then (rw := word ()
		     ; ri := 0)
	    else ri := 1 + i
      in b
      end
end

fun int () = Word.toIntX (word ())

val int = Trace.trace ("Random.int", Unit.layout, Int.layout) int

val maxInt = Int.maxInt

fun nat () = Word.toInt (Word.andb (word (), Word.fromInt maxInt))

val nat = Trace.trace ("Random.nat", Unit.layout, Int.layout) nat

val maxIntR = Real.fromInt maxInt

fun scale r = r / maxIntR

val natReal = Real.fromInt o nat

val natReal = Trace.trace0 ("Random.natReal", Real.layout) natReal

fun real () = scale (natReal () + scale (natReal ()))

val real = Trace.trace0 ("Random.real", Real.layout) real

fun intRange (lo: int, hi: int): unit -> int =
   let
      val rlo = Real.fromInt lo
      val R = Real.fromInt hi - rlo + 1.0
   in
      fn () => Real.trunc (rlo + R * real ())
   end

val intRange =
   Assert.assertFun2
   ("Random.intRange", intRange,
    fn (lo, hi) =>
    (lo <= hi, fn () => (true, fn n => lo <= n andalso n <= hi)))

fun nRandom {list, length, n} =
   let
      fun loop (need: int, length: int, xs: 'a list, ac: 'a list): 'a list =
	 (Assert.assert ("Random.nRandom", fn () => need <= length)
	  ; if need <= 0
	       then ac
	    else (case xs of
		     [] => Error.bug "nRandom"
		   | x :: xs =>
			if intRange (0, length - 1) () < need
			   then loop (need - 1, length - 1, xs, x :: ac)
			else loop (need, length - 1, xs, ac)))
   in loop (n, length, list, [])
   end

val nRandom = fn x =>
   Assert.assertFun
   ("nRandom", nRandom,
    fn {list, length, n} => (length = List.length list
			     andalso 0 <= n
			     andalso n <= length,
			     fn l => n = List.length l))
   x

fun list l =
   let val n = List.length l
   in if n = 0
	 then NONE
      else SOME (List.nth (l, intRange (0, n - 1) ()))
   end

end
