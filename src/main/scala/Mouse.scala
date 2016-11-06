package xyz.hyperreal.mouse

import java.io._

import collection.mutable.ArrayStack


object Mouse extends App
{
	interp( new StringReader("'A !'") )
	
	private class Holder( name: Char )
	{
		var v: Int = _
	}
	
	def interp( input: Reader, init: Int* )
	{
	val lines = new LineNumberReader( input )
	
		lines setLineNumber 1
		
	val r = new PushbackReader( lines )
	val stack = new ArrayStack[Int]
	
		for (n <- init)
			stack push n
			
	val vars = new Array[Int]( 26 )
	var ch = 0
	
		while ({ch = r.read; ch > -1 && ch != '$'})
		{
			ch match
			{
				case '+' | '*' | '-' | '/' | '<' | '>' | '=' =>
					stack.push( ch match
					{
						case '+' => stack.pop + stack.pop
						case '*' => stack.pop*stack.pop
						case '-' => -stack.pop + stack.pop
						case '/' =>
							val top = stack.pop
							
							stack.pop/top
						case '\\' =>
							val top = stack.pop
							
							stack.pop%top
						case '<' => if (stack.pop > stack.pop) 1 else 0
						case '>' => if (stack.pop < stack.pop) 1 else 0
						case '=' => if (stack.pop == stack.pop) 1 else 0
					} )
				case '!' =>
					val c = r.read
					
					if (c == ''')
						println( stack.pop.toChar )
					else
					{
						if (c > -1)
							r.unread( c )
							
						println( stack.pop )
					}
				case _ if ch.toChar.isLetter => stack.push( ch.toChar.toUpper - 'A' )
				case '.' =>
					val n = stack.pop
					
					if (n < 0 || n > 25)
						sys.error( "variable address out of range: " + n )
					else
						stack.push( vars(n) )
				case ':' =>
					val n = stack.pop
					
					if (n < 0 || n > 25)
						sys.error( "variable address out of range: " + n )
					else
						vars(n) = stack.pop
				case _ if ch.toChar.isDigit =>
					var n = ch - '0'
					
					while ({ch = r.read; ch.toChar.isDigit})
						n = n*10 + ch - '0'
					
					stack.push( n )
					
					if (ch > -1)
						r.unread( ch )
				case ''' =>
					val c = r.read
					
					if (c == -1) sys.error( "unexpected end of input" )
					
					stack.push( c )
				case ' ' | '\n' | '\r' | '\t' =>
				case _ => sys.error( "unrecognized character: " + ch )
			}
		}
	
		stack
	}
}