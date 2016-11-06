package xyz.hyperreal.mouse

import java.io._

import collection.mutable.{ArrayStack, ArrayBuffer, HashMap}


object Mouse2 extends App {
	private class Address( val name: String ) {
		var v: Any = 0
		
		override def toString = name
	}
	
	private class Stack extends ArrayStack[Any] {
		def n = pop.asInstanceOf[Int]
	}
	
	private class Env( init: Array[Any] ) {
		val stack = new Stack
		val control = new ArrayStack[Any]
		val vars = new HashMap[String, Address]
		var pc = 0
		var trace = false
		
		def address( name: String ) = {
			vars.get( name ) match {
			case None => 
				val res = new Address( name )
				
				vars += name -> res
				res
			case Some( h ) => h
			}
		}
		
		def address( n: Int ): Address = {
			require( n >= 0 && n < 26, "variable address out of range: " + n )
			
			address( (n + 'A').toChar.toString )
		}
		
		for (n <- init)
			stack push n
	}
	
	abstract class Instruction( code: String ) {
		def execute( sys: Env )
		
		override def toString = code
	}
	
	private object TraceOn extends Instruction( "trace on" ) {
		def execute( env: Env ) = env.trace = true
	}
	
	private object TraceOff extends Instruction( "trace off" ) {
		def execute( env: Env ) = env.trace = false
	}
	
	private object Read extends Instruction( "read" ) {
		def execute( env: Env ) = env.stack.push( io.StdIn.readInt )
	}
	
	private object ReadChar extends Instruction( "read character" ) {
		def execute( env: Env ) = env.stack.push( io.StdIn.readLine.head.toInt )
	}
	
	private object Display extends Instruction( "display" ) {
		def execute( env: Env ) = print( env.stack.pop )
	}
	
	private object DisplayChar extends Instruction( "display character" ) {
		def execute( env: Env ) = print( env.stack.n.toChar )
	}
	
	private class DisplayStr( s: String ) extends Instruction( "display string \"" + s + '"' ) {
		def execute( env: Env ) = print( s )
	}
	
	private object Exit extends Instruction( "exit" ) {
		def execute( env: Env ) = env.pc = -2 // pc is bumped before being checked
	}
	
	private object Leave extends Instruction( "leave" ) {
		def execute( env: Env ) {
			if (env.control.isEmpty || !env.control.top.isInstanceOf[Loop]) sys.error( "not inside loop" )

			if (env.stack.n <= 0)
				env.pc = env.control.pop.asInstanceOf[Loop].exit - 1
		}
	}
	
	private class BranchFalse( location: Int ) extends Instruction( "branch false " + location ) {
		def execute( env: Env ) {
			if (env.stack.n <= 0)
				env.pc = location - 1
		}
	}
	
	private class Branch( location: Int ) extends Instruction( "branch " + location ) {
		def execute( env: Env ) {
				env.pc = location - 1
		}
	}
	
	private class LoopBegin( exit: Int, index: Int, step: Int, limit: Option[Int] ) extends Instruction( "begin" ) {
		def execute( env: Env ) = env.control.push( new Loop(exit, index, step, limit) )
	}
	
	private object LoopEnd extends Instruction( "end" ) {
		def execute( env: Env ) {
			env.control.top.asInstanceOf[Loop].index += 1
		}
	}
	
	private class Variable( name: String ) extends Instruction( name ) {
		def execute( env: Env ) =
			env.stack.push(
				if (name.length == 1)
					name(0) - 'A'
				else
					env.address( name )
			)
	}
	
	private object Fetch extends Instruction( "fetch" ) {
		def execute( env: Env ) = {
		val v = env.stack.pop
			
			v match {
				case n: Int => env.stack.push( env.address(n).v )
				case a: Address => env.stack.push( a.v )
				case _ => sys.error( "expected variable address: " + v )
			}
		}
	}
	
	private object Store extends Instruction( "store" ) {
		def execute( env: Env ) = {
		val a = env.stack.pop
		val v = env.stack.pop
		
			a match {
				case n: Int => env.address(n).v = v
				case a: Address => a.v = v
				case _ => sys.error( "expected variable address: " + v )
			}
		}
	}
	
	private object IndexI extends Instruction( "index i" ) {
		def execute( env: Env ) = env.stack.push( env.control.top.asInstanceOf[Loop].index )
	}
	
	private abstract class Push( code: String ) extends Instruction( code ) {
		def execute( env: Env ) = env.stack.push( push(env.stack) )
		
		def push( st: Stack ): Any
	}
	
	private class Function( f: Stack => Any, code: String ) extends Push( code ) {
		def push( st: Stack ) = f( st )
	}
	
	private class Number( n: Int ) extends Function( _ => n, n toString )
	
	private class Chr( c: Char ) extends Function( _ => c.toInt, "'" + c )
	
	private object Add extends Function( st => st.n + st.n, "add" )
	
	private object Sub extends Function( st => -st.n + st.n, "sub" )
	
	private object Mul extends Function( st => st.n * st.n, "mul" )
	
	private object Div extends Push( "div" ) {
		def push( st: Stack ) = {
		val top = st.n
			
			st.n/top
		}
	}
	
	private object Rem extends Push( "rem" ) {
		def push( st: Stack ) = {
		val top = st.n
			
			st.n%top
		}
	}
	
	private object Dup extends Function( st => st.top, "dup" )
	
	private object Over extends Function( st => st(1), "over" )
	
	private object Swap extends Instruction( "swap" ) {
		def execute( env: Env ) {
		val first = env.stack.pop
		val second = env.stack.pop

			env.stack.push( first )
			env.stack.push( second )
		}
	}
	
	private class Predicate( p: Stack => Boolean, code: String ) extends
		Function( st => if (p( st )) 1 else 0, code )
	
	private object Lt extends Predicate( st => st.n > st.n, "lt" )
	
	private object Gt extends Predicate( st => st.n < st.n, "gt" )
	
	private object Eq extends Predicate( st => st.n == st.n, "eq" )
	
	private class Loop( val exit: Int, var index: Int, val step: Int, val limit: Option[Int] )
	
	def run( program: IndexedSeq[Instruction], init: Any* ) = {
	val env = new Env( init.toArray )		
		
		while (env.pc >= 0 && env.pc < program.length) {
		val inst = program(env.pc)
		
			inst.execute( env )
			
			if (env.trace) {
				println( env.pc, inst, env.stack.toIndexedSeq, env.vars.map(kv => kv._1 -> kv._2.v).toMap )
			}
			
			env.pc += 1
		}
	
		(env.stack.toIndexedSeq, env.vars.map(kv => kv._1 -> kv._2.v).toMap)
	}
	
	def run( input: Reader, init: Any* ): (IndexedSeq[Any], Map[String, Any]) = run( compile(input), init :_* )
	
	private val TOP = 0
	private val CONDITIONAL = 1
	private val LOOP = 2
	
	def compile( input: Reader ) = {
	val lines = new LineNumberReader( input )
	
		lines setLineNumber 1
		
	val r = new PushbackReader( lines )
	val program = new ArrayBuffer[Instruction]
	val stack = new ArrayStack[Int]
	
		def add( inst: Instruction ) = program += inst
		
		def inst: Int = {
		var ch = r.read
	
			if (ch > -1) {
				ch match {
					case '$' => program += Exit
					case '+' => program += Add
					case '-' => program += Sub
					case '*' => program += Mul
					case '/' => program += Div
					case '\\' => program += Rem
					case '<' => program += Lt
					case '>' => program += Gt
					case '=' => program += Eq
					case _ if ch.toChar.isDigit =>
						var n = ch - '0'
						
						while ({ch = r.read; ch.toChar.isDigit})
							n = n*10 + ch - '0'
						
						program += new Number( n )
						
						if (ch > -1)
							r.unread( ch )
					case '!' =>
						val c = r.read
						
						if (c == ''')
							program += DisplayChar
						else
						{
							if (c > -1)
								r.unread( c )
								
							program += Display
						}
					case '[' =>
						stack.push( CONDITIONAL )
						
						val branch = program.length
						
						program += null
						
						if (inst != CONDITIONAL) sys.error( "unclosed conditional" )
						
						program(branch) = new BranchFalse( program.length )
					case ']' =>
						if (stack.isEmpty || stack.pop != CONDITIONAL) sys.error( "unexpected end of conditional" )
						
						return CONDITIONAL
					case '(' =>
						stack.push( LOOP )
						
						val begin = program.length
						
						program += null
						
						val start = program.length
						
						if (inst != LOOP) sys.error( "unclosed loop" )
						
						program += LoopEnd
						program += new Branch( start )
						program(begin) = new LoopBegin( program.length, 0, 1, None )
					case ')' =>
						if (stack.isEmpty || stack.pop != LOOP) sys.error( "unexpected end of loop" )
						
						return LOOP
					case '^' => program += Leave
					case '.' => program += Fetch
					case ':' => program += Store
					case '"' =>
						val s = new StringBuilder
						
						while ({ch = r.read; ch > -1 && ch != '"'})
							if (ch == '!')
								s append '\n'
							else
								s append ch.toChar
						
						if (ch == -1) sys.error( "unexpected end of input" )
							
						program += new DisplayStr( s toString )
					case _ if ch.toChar.isLetter || ch == '_' =>
						val n = new StringBuilder( ch.toChar.toString )
						
						while ({ch = r.read; ch > -1 && (ch.toChar.isLetter || ch == '_')})
							n append ch.toChar
						
						val name = n.toString.toUpperCase
						
						if (name startsWith "_")
						{
							name match
							{
								case "_I" => program += IndexI
								case "_SWAP" => program += Swap
								case "_DUP" => program += Dup
								case "_OVER" => program += Over
							}
						}
						else
							program += new Variable( name )
						
						if (ch > -1)
							r.unread( ch )
					case ''' =>
						val c = r.read
						
						if (c == -1) sys.error( "unexpected end of input" )
						
						program += new Chr( c.toChar )
					case '{' => program += TraceOn
					case '}' => program += TraceOff
					case ' ' | '\n' | '\r' | '\t' =>
					case '~' =>
						while ({ch = r.read; ch > -1 && ch != '\r' && ch != '\n'}) {}
						
						if (ch > -1)
							r.unread( ch )
					case '?' =>
						val c = r.read
						
						if (c == ''')
							program += ReadChar
						else
						{
							if (c > -1)
								r.unread( c )
								
							program += Read
						}
					case _ => sys.error( "unrecognized character: " + ch )
				}
				
				inst
			}
		
			TOP
		}
	
		inst
		program toIndexedSeq
	}
}