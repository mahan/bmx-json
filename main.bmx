
Function fileToUniString:String(url:String)
	Local f:TStream = ReadFile(url)
	Local s:String
	While Not Eof(f)
		s:+ReadLine(f)
	WEnd
	f.Close()
	Return s
End Function


Function Main()
	Local s:String = fileToUniString("test.txt")
	
	Print "s: " + s
	Print "utf8 converted: " + Utf8ToBmxString(s)
	Print "There and back again: " + bmxStrToUtf8(Utf8ToBmxString(s))

	Print "Empty string to utf8: '" + bmxStrToUtf8("") + "'"
	Print "Empty string from utf8: '" + Utf8ToBmxString("") + "'"
	
	s = fileToUniString("test.json")
	
	Local json:TJsonNode = TJsonNode.Create(s)

	Print json.getValueFromPath("layers/0/data/length")

	Print json.getValueFromPath("layers/0/data/0")
	   
	Print json.getValueFromPath("layers/0/height")

	Local sum:Int
	For Local i:Int = 0 Until json.getValueFromPath("layers/0/data/length").ToInt()
		sum:+json.getValueFromPath("layers/0/data/" + i).ToInt()
	Next
	
	Print "sum: " + sum
	
	Print "finished"
End Function

Main()
