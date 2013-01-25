' ****************************************************************************
'  Copyright (c) 2013 Mattias Hansson 
'  GNU GPL v2
'
' This file implements a self-contained JSON parser/"xpath"/encoder(not yet!)
'
' Usage:
'   'Parse the JSON data (utf8 string)
'	Local json:TJsonNode = TJsonNode.Create(s)
'   'next extract and use the data in a XPATH fashion
'   Print json.getValueFromPath("layers/0/data/length")
'
' Notes(0):
'   JSON arrays are parsed so that values are given the names 0..n
'   An additional property in arrays is the 'length'-property that contains
'   the current number of elements in the array (just as in JavaScript)
'
' Notes(1):
'   In some situations the parser might be more lenient than the spec, but 
'   it should always accept correct JSON.
'
' ****************************************************************************


'Converts a BlitzMax string to utf8
'
Function bmxStrToUtf8:String(s:String)
	Local r:String
	For Local i:Int = 0 Until s.length
		Local char:Int = s[i]
		If char < 128
			r:+Chr(char)
		Else If char < 2048
			r:+Chr(char / 64 | 192)
			r:+Chr(char Mod 64 | 128)
		Else
			r:+Chr(char / 4096 | 224)
			r:+Chr(char / 64 Mod 64 | 128)
			r:+Chr(char Mod 64 | 128)
		EndIf
	Next
	Return r
End Function

'Converts a utf8 string to a blitzmax string
'
Function Utf8ToBmxString:String(s:String)
		Local buf:Short[s.length]
		Local sPos:Int
		Local bPos:Int = -1
		
		While sPos < s.Length
			bPos:+1
			
			Local c:Int = s[sPos]
			sPos:+1
			If c < 128
				buf[bPos] = c
				Continue
			EndIf

			Local d:Int = s[sPos]
			sPos:+1
			If c < 224
				buf[bPos] = (c - 192) * 64 + (d - 128)
				Continue
			EndIf
			
			Local e:Int = s[sPos]
			sPos:+1
			If c < 240
				buf[bPos] = (c - 224) * 4096 + (d - 128) * 64 + (e - 128)
				Continue
			EndIf
			
			buf[bPos] = 0 '?
		EndWhile
		Return String.FromShorts(buf, bPos + 1)
End Function

Type TJsonNode Extends TMap

	Function _parseError(msg:String, tok:TStringTokenStreamer)
		Local excerpt:String = Mid(tok.str, tok.pos - 5, 10)
		Throw msg + ". Pos: " + tok.pos + " (tot)Length: " + tok.str.Length + " excerpt: '" + excerpt + "'"
	End Function

	Method _parse(tok:TStringTokenStreamer) Abstract

	Function Create:TJsonNode(utf8:String)
		Local tok:TStringTokenStreamer = TStringTokenStreamer.Create(Utf8ToBmxString(utf8))
		Local r:TJsonNode
		Select tok.nextNonWsChar()
			Case "{"
				r = New TJsonObject
			Case "["
				r = New TJsonArray
			Default
				_parseError "<Json> expects '{' or '['.", tok
		End Select
		r._parse(tok)
		Return r
	EndFunction

	Method _parseString:String(tok:TStringTokenStreamer)
		'DebugLog "_parseString() ->"
		Local qmode:Int = False
		Local c:String
		Local r:String
		Repeat
			c = tok.nextChar()
			If c = "" Then _parseError "<Json> unterminated string", tok
			If c = "~q" And Not qmode Then Exit
			If Not qmode And c = "\" Then qmode = True
			
			Select qmode
				Case True
					Select tok.nextChar()
						Case "~q", "\", "/"
							r:+tok.previousChar()
						Case "b"
							r:+Chr($08)
						Case "f"
							r:+Chr($0c)
						Case "n"
							r:+Chr($0a)
						Case "r"
							r:+Chr($0d)
						Case "t"
							r:+Chr($09)
						Case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "a", "b", "c", "d", "e", "f"
							Local _hex:String = tok.previousChar() + tok.nextChar() + tok.nextChar() + tok.nextChar()
							If Len(_hex) < 4 Then _parseError "<Json> expected 4 hexadecimal digits", tok
							r:+Chr(Int("$" + _hex))
						Default
							_parseError "<Json> expected '~q', 'b', 'f', 'n', 'r', 't' or 4 hexadeimal digits", tok
					End Select
					qmode = False
				Case False
					r:+c
			End Select
			
		Forever
		'DebugLog "_parseString() <- :" + r
		Return r
	End Method
	
	'quite naive parser making use of bmx built in capabilities, without checking for syntax correctness.
	Method _parseNumber:String(tok:TStringTokenStreamer)
		'DebugLog "_parseNumber() ->"
		
		'Gold parser regexp base:
		'Number = '-'?('0'|{Digit9}{Digit}*)('.'{Digit}+)?([Ee][+-]?{Digit}+)?
		
		
		'Working but only int parser:
		'---8<------		
		Rem
		
		Const DIGITS:String = "0123456789"
		Local num:String = tok.previousChar()
		While DIGITS.Contains(tok.lookAheadChar())
			num:+tok.nextChar()
		EndWhile
		
		EndRem
		
		'---8<------		

		
		
		Local num:String = tok.previousChar()
		
		While "0123456789.eE+-".Contains(tok.lookAheadChar())
			num:+tok.nextChar()
		WEnd

		Local intVal:Int = num.ToInt()
		Local floatVal:Double = num.ToDouble()
		
		Const EPSILON:Double = 0.1E-6
				
		If Abs(floatVal - intVal) < EPSILON
			num = String.FromInt(intVal)
		Else
			num = String.FromDouble(floatVal)
		EndIf		
		
		'DebugLog "_parseNumber() <- :" + num
		Return num

	EndMethod
	
	Method _parseValue(tok:TStringTokenStreamer, putInKey:String)
		'DebugLog "inserts to " + putInKey
		Select tok.nextNonWsChar()
			Case "~q"
				Insert(putInKey, _parseString(tok))
			Case "-", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
				Insert(putInKey, _parseNumber(tok))
			Case "{"
				Local o:TJsonObject = New TJsonObject
				o._parse(tok)
				Insert(putInKey, o)
			Case "["
				Local a:TJsonArray = New TJsonArray
				a._parse(tok)
				Insert(putInKey, a)
			Case "t" 'true
				Local t:String = tok.previousChar() + tok.nextChar() + tok.nextChar() + tok.nextChar()
				If Not t = "true" Then _parseError "<Json> Value. Expected 'true'", tok
				Insert(putInKey, "true")
			Case "f" 'false
				Local t:String = tok.previousChar() + tok.nextChar() + tok.nextChar() + tok.nextChar() + tok.nextChar()
				If Not t = "false" Then _parseError "<Json> Value. Expected 'false'", tok
				Insert(putInKey, "false")
			Case "n" 'null
				Local t:String = tok.previousChar() + tok.nextChar() + tok.nextChar() + tok.nextChar()
				If Not t = "null" Then _parseError "<Json> Value. Expected 'null'", tok
				Insert(putInKey, Null)
			Default
				_parseError "<Json> Value. Expected '~q', '-', '0'-'9', '{', '[', 't', 'f', 'n'", tok
		End Select
	EndMethod
	
	Method _parsePair(tok:TStringTokenStreamer)
		Local key:String = _parseString(tok)
		If Not tok.nextNonWsChar() = ":" Then _parseError "<Json> expects ':'", tok
		_parseValue(tok, key)
	EndMethod
	
	Method strJoin:String(bits:String[], sep:String)
		Local r:String
		For Local bit:String = EachIn bits
			If r
				r:+sep + bit
			Else
				r = bit
			End If
		Next
		Return r
	End Method
	
	'get value from path
	Method xPath:String(path:String)
		'DebugLog "path: " + path
		Local valNames:String[] = path.Split("/")
				
		If valNames[0] = "" Return "<TJsonNode> Object"
		If Contains(valNames[0])
			If String(ValueForKey(valNames[0]))
				Return String(ValueForKey(valNames[0]))
			ElseIf TJsonNode(ValueForKey(valNames[0]))
				Return TJsonNode(ValueForKey(valNames[0])).xPath(strJoin(valNames[1..], "/"))
			ElseIf Not ValueForKey(valNames[0]) ' Null
				Return "null"
			Else
				Return "Unknown type?" 'should not happen!
			End If
		Else
			Return ""
		EndIf
	End Method
	
EndType

Type TJsonObject Extends TJsonNode

	Method _parse(tok:TStringTokenStreamer)
		While Not tok.Eof()
			Select tok.nextNonWsChar()
				Case "~q"
					_parsePair(tok)
					tok.swallowIfChar(",")
				Case "}"
					Return
				Default
					_parseError "<Json> expects '~q' (key/value pair) or '}'", tok
			End Select
		EndWhile
	EndMethod
	
EndType

Type TJsonArray Extends TJsonNode
	Field arrayCount:Int

	Method _parse(tok:TStringTokenStreamer)
		While Not tok.Eof()
			Select tok.lookAheadChar()
				Case "]"
					tok.nextChar() 'swallow the end char
					Return
				Default
					_parseValue(tok, String.FromInt(arrayCount))
					arrayCount:+1
					Insert("length", String.FromInt(arrayCount))
					tok.swallowIfChar(",")
			End Select
		WEnd

	EndMethod
	
EndType


'
' This class allows for reading a string a char at a time and other nifty things that go well in parsing text.
'

Type TStringTokenStreamer

	Const WHITESPACE:String = Chr($20) + Chr($09) + Chr($0a) + Chr($0d) 'Space + Horizontal tab + Line feed Or New line + Carriage Return
	
	Field str:String
	Field pos:Int

	Function Create:TStringTokenStreamer(s:String)
		Local r:TStringTokenStreamer = New TStringTokenStreamer
		r.str = s
		r.pos = 0
		Return r
	End Function

	Method nextChar:String()
		If Eof() Return ""
		Local c:String = Chr(str[pos])
		pos:+1
		Return c
	End Method

	Method nextNonWsChar:String()
		Local c:String
		Repeat
			c = nextChar()
		Until Not WHITESPACE.Contains(c)
		Return c
	End Method
	
	Method previousChar:String()
		If pos - 1 > 0 Then Return Chr(str[pos - 1])
		Return ""
	End Method
	
	Method swallowIfChar(char:String)
		If char <> nextNonWsChar() Then pos:-1
		If pos < 0 Then pos = 0
	EndMethod
	
	Method lookAheadChar:String()
		If Not Eof() Then Return Chr(str[pos])
		Return ""
	End Method
	
	Method Eof:Int()
		Return (pos >= str.Length)
	End Method
EndType
