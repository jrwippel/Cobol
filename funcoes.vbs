function VB_FormataCampo( strValor, cMascara, xMascara )
	dim retorno 
	dim byteMasc 
	dim byteCampo 
	dim cElemPuro
	
	cElemPuro = RemoveLixo( strValor, cMascara, xMascara )
	retorno = ""
	
	for ind = 1 to Len( cMascara )		    
		if  Len( cElemPuro ) > 0 then		    
			byteMasc = UCase( Mid( cMascara, ind, 1 ) )
			if  byteMasc <> "A" and byteMasc <> "X" and byteMasc <> "9" and byteMasc <> "#" then
				retorno = retorno + Mid( cMascara, ind, 1 )
			else				
				byteCampo = Mid( cElemPuro, 1, 1 )				
				if  ( not IsAlpha( byteCampo ) and not IsNumeric( byteCampo ) ) then
					byteCampo = ""				
				end if
				if  byteMasc = "A" then
					if  IsAlpha( byteCampo ) then
						retorno = retorno + byteCampo
					end if
				end if
				if  byteMasc = "9" then
					if  IsNumeric( byteCampo ) then
						retorno = retorno + byteCampo
					end if
				end if
				if  byteMasc = "X" then
					if  IsNumeric( byteCampo ) or IsAlpha( byteCampo ) then
						retorno = retorno + byteCampo
					end if
				end if
				if  byteMasc = "#" then
					retorno = retorno + Mid( xMascara, 1, 1 )
					xMascara = Mid( xMascara, 2 )
				else
					cElemPuro = Mid( cElemPuro, 2 )
				end if				
			end if 
		else
			exit for
		end  if
	next 

	VB_FormataCampo = retorno
end function

function IsAlpha( str ) 
	if  ( ( str >= "a" ) and ( str <= "z" ) ) or ( ( str >= "A" ) and ( str <= "Z" ) ) then
		IsAlpha = true
	else
		IsAlpha = false
	end if 
end function

function UndoKeyChars( str )
	dim retorno
	retorno = Ucase( str )
	retorno = Replace( retorno, "9", " " )
	retorno = Replace( retorno, "A", " " )
	retorno = Replace( retorno, "X", " " )
	UndoKeyChars = retorno
end function	

function RemoveLixo( str, cMascara, xMascara )
	dim cMascEdit
	dim cReplace
	cReplace = xMascara
	cMascEdit = UndoKeyChars( cMascara )
	for nIndex = 1 to Len( cMascara )
		if  Mid( cMascara, nIndex, 1 ) = "#" and Mid( cReplace, 1, 1 ) = Mid( str, nIndex, 1 ) then
		    str = Mid( str, 1, nIndex-1 ) & " " & Mid( str, nIndex+1 )
		    cReplace = Mid( cReplace, 2 )
		end if
	next
	for nIndex = 1 to Len( cMascEdit )	    
		str = Replace( str, Mid( cMascEdit, nIndex, 1 ), "" )
	next
	RemoveLixo = str
end function

function VB_Mensagem( strMensagem, nOpc, strTitle )
	VB_Mensagem = MsgBox( strMensagem, nOpc, strTitle )
end function

function VB_Chr( Valor )
	VB_Chr = Chr( Valor )
end function

Function VB_ValidaCNPJ( Valor )
	Dim Mult1
	Dim Mult2
	Dim dig1
	Dim dig2
	Dim X
	Valor = Trim( Valor )
	If Len( Valor ) <> 14 then
		VB_ValidaCNPJ = false
		Exit Function
	end if
	Mult1 = "543298765432"
	Mult2 = "6543298765432"
	For X = 1 To 12
	    dig1 = dig1 + ( CDbl( Mid( Valor, X, 1 ) ) * CDbl( Mid( Mult1, X, 1 ) ) )
	Next
	For X = 1 To 13
	    dig2 = dig2 + ( CDbl( Mid( Valor, X, 1 ) ) * CDbl( Mid( Mult2, X, 1 ) ) )
	Next
	dig1 = ( dig1 * 10 ) Mod 11
	dig2 = ( dig2 * 10 ) Mod 11
	If dig1 = 10 Then dig1 = 0
	If dig2 = 10 Then dig2 = 0
	VB_ValidaCNPJ = True
	If CDbl( dig1 ) <> CDbl( Mid( Valor, 13, 1 ) ) Then VB_ValidaCNPJ = False
	If CDbl( dig2 ) <> CDbl( Mid( Valor, 14, 1 ) ) Then VB_ValidaCNPJ = False
End Function

function getNewSessionID()
                     con = "bcdfghjklmnpqrstvyxzw"
                     num = "1234567890"
                     vog = "aeiouAEIOUCKIJKL"
                     lcon = len(con)
                     lnum = len(num)
                     lvog = len(vog)
                     randomize
                     rcon = (RND * lcon + 1)
                     rnum = (RND * lnum + 1)
                     rvog = (RND * lvog + 1)
                     randomize
                     rcon = cint(rcon)
                     rnum = cint(rnum)
                     rvog = cint(rvog)
                     randomize
                     senha =  mid(con, rcon, 1) & mid(num, rnum, 1) & mid(vog, rvog, 1) & mid(num, (rnd * rnum + 1), 1) & mid(vog, (rnd * rvog + 1), 1) & mid(con, (rnd * rcon + 1), 1)
                     randomize
                     MyValue = Int(("67891346" * Rnd) + 1)   ' Generate random value between 1 and 6.
                     getNewSessionID = senha & MyValue
End function

Function VB_DateDiff( dData1, dData2 )
	if  isEmpty( dData2 ) or dData2 = "" then
		VB_DateDiff = DateDiff( "d", "01/01/1900", dData1 ) + 2
	else
		VB_DateDiff = DateDiff( "d", dData1, dData2 )
	end if
End Function

Function VB_Replace( string, strFind, strReplace )
	VB_Replace = Replace( string, strFind, strReplace )
End Function

Function VB_Round( vlr, dec )
	if  dec > 16 then
		dec = 16
	end if
	VB_Round = Round( vlr, dec )
End Function

Function VB_DateAdd( qtdeDias, strData, strPar )
	dim data 
	data = DateAdd( strPar, qtdeDias, strData )
	VB_DateAdd = formatdatetime( data, 2 )
End Function