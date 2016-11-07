package xyz.hyperreal.mouse

import java.io._


object TestMain extends App
{
	import Mouse._
	
	val code =
		"""
~ General test program for Mouse interpreters
"!Arithmetic Expressions: "
   777 223 + !   "="  3579 2579 - ! "="
   25 40 * !     "="  7000 7 / !    "="
   5137 1379 \ ! "=1000!"
"Boolean Expressions: "
   789 891 > !   "=" 0 17 - 36 > !  "="  ~ False expressions
   0 1 = ! "="   0 1000 - 1000 = !  "="
   6 5 < ! "=" 1000 0 1000 - < !    "="
   17 18 > 5 5 = * ! "="
   17 18 > 5 6 > + ! "=0    "
   1 0 > ! "=" 0 0 1 - > !   "="         ~ True expressions
   0 0 = ! "=" 1000 1000 = ! "="
   0 17 - 37 < ! "=" 0 1 < ! "="
   17 18 < 6 5 > * ! "="
   5 6 > 1 1 = + ! "=1!"
"Assignments: "
   1000 A: A. ! "=" A. Z: Z. ! "="
   A 25 + . ! "=1000!"
"Conditional Statements: "
   1 0 > [ "OK" ]
   1 0 = [ " *** Error ***" ] "!"
"Loops: "
   0 N:
   ( N. ! " " N. 10 < ^ N. 1 + N: )
   "!!   "
   0 M:                                  ~ Multiplication table
   ( 0 N:
      ( M. N. * ! " "
         N. 5 < ^ N. 1 + N: )
   "!   " M. 5 < ^ M. 1 + M: ) "!"
		"""
	val prog = compile( new StringReader(code) )
	
//	println( prog )
	println( "\n" + run(prog) )
}