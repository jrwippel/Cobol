function CadastraHelp( janela, cgiPath ){
	strUrl = cgiPath + "&call=PP00004&opcao=1&janela=" + janela + "&SubSessionID=" + getNewSessionID();
	opcoes = "left=05,height=480,width=780,top=20,status=yes,toolbar=no,menubar=no,location=no";
	window.open( strUrl, null, opcoes );
}
function MostraHelp( janela, cgiPath ){
	strURL = cgiPath + "&call=PP00004&opcao=2&janela=" + janela + "&SubSessionID=" + getNewSessionID();
	opcoes = "dialogHeight: 560px; dialogWidth: 780px;center: Yes; help: No; resizable: No; status: No;";
	window.showModalDialog( strURL, null, opcoes );
}
function Mensagem( strMensagem, nOpc, strTitle ){
	return( VB_Mensagem( strMensagem, nOpc, strTitle ) );
}
function AltPesquisa( strAcao ){
	if( event.altKey && ( event.keyCode == 80 || event.keyCode == 112 ) ){
		eval( strAcao );
	}
}
function VerificaTeclaPress( xTecla, strAcao ) {
	if( event.keyCode == xTecla ) {
		eval( strAcao );
	}
}
function VerificaTeclaTAB( strAcao ) {
	if( event.keyCode == 9 ) {
		eval( strAcao );
	}
}
function ValidaData( dateStr, tipodata ){
	datePat = /^(\d{1,2})(\/|-)(\d{1,2})\2(\d{4})$/; 
	var matchArray = dateStr.match( datePat ); 
	if( matchArray == null ){
		alert( 'Data inv�lida' );
		return false;
	}
	day = matchArray[1];
	month = matchArray[3];
	year = matchArray[4];
	if( month < 1 || month > 12 ){
		alert( 'Data inv�lida' );
		return false;
	}
	if( day < 1 || day > 31 ){
		alert( 'Data inv�lida' );
		return false;
	}
	if( ( month==4 || month==6 || month==9 || month==11) && day==31 ){
		alert( 'Data inv�lida' );
		return false;
	}
	if( month == 2 ){
		var isleap = ( year % 4 == 0 && ( year % 100 != 0 || year % 400 == 0 ) );
		if( day>29 || ( day==29 && !isleap ) ){
			alert( 'Data inv�lida' );
			return false;
		}
	}
	if( year < 1000 || year >= 3000 ){
		alert( 'Data inv�lida' );
		return false;
	}
	return true;
}
function ValidaDMA( dateStr, tipoData ){
	if( tipoData == 'D' && ( dateStr < 1 || dateStr > 31 ) ){
		alert( 'Dia inv�lido' );
		return false;
	}
	if( tipoData == 'M' && ( dateStr < 1 || dateStr > 12 ) ){
		alert( 'M�s inv�lido' );
		return false;
	}
	if( tipoData == 'A' && ( dateStr < 1000 || dateStr > 2999 ) ){
		alert( 'Ano inv�lido' );
		return false;
	}
	return true;
}
function ValidaMesAno(mesAno) {
	if( mesAno.length != 7 ){
		alert( 'Data inv�lida' );
		return false;
	}
	if( ! ValidaData( '01/' + mesAno, 'DD/MM/AAAA' ) ){
		return false;
	}
	return true;
}
function DataMaior( dateIni, dateFim ){
	var datePat = /^(\d{1,2})(\/|-)(\d{1,2})\2(\d{4})$/;
	var matchArray = dateFim.match( datePat ); 
	ddFim = matchArray[1];
	mmFim = matchArray[3]; 
	aaFim = matchArray[4];
	if( ddFim < 10 ){
		ddFim = "0" + ddFim;
	}
	if( mmFim < 10 ){
		mmFim = "0" + mmFim;
	}
	dateFim = aaFim + mmFim + ddFim;
	var matchArray = dateIni.match( datePat ); 
	ddIni = matchArray[1];
	mmIni = matchArray[3]; 
	aaIni = matchArray[4];
	if( ddIni < 10 ){
		ddIni = "0" + ddIni;
	}
	if( mmIni < 10 ){
		mmIni = "0" + mmIni;
	}
	dateIni = aaIni + mmIni + ddIni;
	if( dateIni > dateFim ){
		return true;
	}
	return false;    
}
function ProcuraPosicao( texto, palavra ){
	var r;
	r = texto.search( palavra );
	return( r );
}
function mid( str, pos, len ){
	return str.substring( pos, pos + len );
}
function replace( string, strFind, strReplace ){
	return( VB_Replace( string, strFind, strReplace ) );
}
function ValidaCNPJ( CNPJ ) {
	CNPJ = CNPJ.replace( ".", "" );
	CNPJ = CNPJ.replace( ".", "" );
	CNPJ = CNPJ.replace( "/", "" );
	CNPJ = CNPJ.replace( "-", "" );
	return VB_ValidaCNPJ( CNPJ );
}
function isNumeric( valor ){
	for( var i = 0 ; i < valor.length ; i++ ){
		if( ( valor.charAt( i ) < '0' ) || ( valor.charAt( i ) > '9' ) ){
			return false;
		}
	}
	return true;
}
function ValidaCPF( CPF ){
	var mx1, mx2, mx3, mx4, mx5, mx6, mx7, mx8, mr, mc1, mc2, mSoma1, mSoma2, mDv1, mDv2;
	CPF = CPF.replace( ".", "" );
	CPF = CPF.replace( ".", "" );
	CPF = CPF.replace( "-", "" );
	if( !isNumeric( CPF ) ) {
		return false;
	}
	if( CPF.length != 11 ) {
		return false;
	}
	mx1 = parseFloat( CPF.charAt(  0 ) );
	mx2 = parseFloat( CPF.charAt(  1 ) );
	mx3 = parseFloat( CPF.charAt(  2 ) );
	mx4 = parseFloat( CPF.charAt(  3 ) );
	mx5 = parseFloat( CPF.charAt(  4 ) );
	mx6 = parseFloat( CPF.charAt(  5 ) );
	mx7 = parseFloat( CPF.charAt(  6 ) );
	mx8 = parseFloat( CPF.charAt(  7 ) );
	mr  = parseFloat( CPF.charAt(  8 ) );
	mc1 = parseFloat( CPF.charAt(  9 ) );
	mc2 = parseFloat( CPF.charAt( 10 ) );
	mSoma1 = mr * 2 + mx8 * 3 + mx7 * 4 + mx6 * 5 + mx5 * 6 + mx4 * 7 + mx3 * 8 + mx2 * 9 + mx1 * 10;
	mDv1 = 11 - ( mSoma1 - ( parseInt( mSoma1 / 11 ) * 11 ) )
	if( mDv1 > 9 ) {
		mDv1 = 0;
	}
	if( mc1 != mDv1 ) {
		return false;
	}
	mSoma2 = mc1 * 2 + mr * 3 + mx8 * 4 + mx7 * 5 + mx6 * 6 + mx5 * 7 + mx4 * 8 + mx3 * 9 + mx2 * 10 + mx1 * 11;
	mDv2 = 11 - ( mSoma2 - ( parseInt( mSoma2 / 11 ) * 11 ) );      
	if( mDv2 > 9 ) {
	  	mDv2 = 0;
	}
	if( mc2 != mDv2 ){
		return false;
	}
	return true;
}
function FormataData( Campo, teclapres )  {                                                     
    var tecla = teclapres.keyCode;                                                              
    vr = Campo.value;                                                                           
    vr = vr.replace( ".", "" );                                                                 
    vr = vr.replace( "/", "" );                                                                 
    vr = vr.replace( "/", "" );                                                                 
    tam = vr.length + 1;                                                                        
    if ( tecla != 9 && tecla != 8 ){                                                            
        if ( tam > 2 && tam < 5 )                                                               
            Campo.value = vr.substr( 0, tam - 2  ) + '/' + vr.substr( tam - 2, tam );           
        if ( tam >= 5 && tam <= 10 )                                                            
            Campo.value = vr.substr( 0, 2 ) + '/' + vr.substr( 2, 2 ) + '/' + vr.substr( 4, 4 );
    }                                                                                           
}   
function SomenteNumeros( str ){
	if( str != undefined ){
		var strAlfa = String.fromCharCode( event.keyCode );
		for( i=0; i < str.length; i++ ){
			if( strAlfa == str.substring( i,i+1 ) ){
				return true;
			}
		}
	}
	if( event.keyCode >= 48 && event.keyCode <= 57 ){
		return true;
	}
	else {
		return false;
	}
}
function FormataValor( Campo, TamMax, TeclaPres, QtdeDecimais ) {
	var Tecla = TeclaPres.keyCode;
	var vr = Campo.value;
	var vrTMP = "";
	var Tam = 0;
	var ValDecimal = "";
	var Aux = 0;
	var noBackSpace = 1;
	
	if( Campo.readOnly ) {
	    return false;
	}
	if( Tecla == 9 || ( Tecla >= 37 && Tecla <= 40 ) ) {
		return true;
	}
	vr = RemoveZerosEsq( vr );
	if( Tecla >= 96 && Tecla <= 105 ) {
		Tecla = Tecla - 48;
	}
	if( Tecla >= 48 && Tecla <= 57 ) {
		if( vr == "" ) { 
			Tam = 1; }
		else {
			Tam = vr.length + 1; }
	}
	else {
		if( Tecla == 8 ) {
			if( vr == "" ) { 
				Tam = 0; }
			else {
				Tam = vr.length - 1; 
			}
			vr = vr.substr( 0, vr.length-1 )
			noBackSpace = 0 }
		else {
			return false;
		}
 	}
	if( Tam > TamMax ) { 
		return false; 
	}
	if( QtdeDecimais > 0 ) {
		if( Tam <= QtdeDecimais ) {
			vr = "0," + Replicate( "0", ( QtdeDecimais - Tam ) ) + vr; }
		else {
			ValDecimal = vr.substr( vr.length - QtdeDecimais + noBackSpace, QtdeDecimais );
			vr = vr.substr( 0, vr.length - QtdeDecimais + noBackSpace );
		}
	}
	if( Tam > QtdeDecimais ) { 
		Aux = vr.length;
		if( QtdeDecimais == 0 && Tecla != 8 ) {
			Aux = Aux + 1;
		}
		while( Aux > 3 ) {
			if( ( vr.substr( 0, Aux-3 ) ).length > 0 ) {
				vrTMP = "." + vr.substr( Aux-3, 3 ) + vrTMP; 
			}
			vr = vr.substr( 0, Aux-3 );
			Aux = Aux-3;
		}
		vr = vr + vrTMP;
		if( QtdeDecimais > 0 ) {
			vr = vr + "," + ValDecimal;
		}
	}
	if( Tecla == 8 ) {
		vrTMP = ""; }
	else {
		vrTMP = String.fromCharCode( Tecla );
	}	
	Campo.value = vr + vrTMP;
	return false;
}
function RemoveZerosEsq( campo ) {
    campo = campo.replace( ".", "" );
    campo = campo.replace( ".", "" );
    campo = campo.replace( ".", "" );
    campo = campo.replace( ".", "" );
    campo = campo.replace( ".", "" );
    campo = campo.replace( ".", "" );
    campo = campo.replace( ".", "" );
    campo = campo.replace( ".", "" );
    campo = campo.replace( ".", "" );
    campo = campo.replace( ".", "" );
    campo = campo.replace( ",", "" );
    campo = campo.replace( ",", "" );
    campo = campo.replace( ",", "" );
	while( true ) {
		if( campo.substr( 0, 1 ) == "0" ) {
			campo = campo.replace( "0", "" ); }
		else {
			return( campo );
		}
	}
}	
function Replicate( Texto, Qtde ) {
var Retorno = "";
    for( var i=0; i < Qtde; i++ ) {
        Retorno = Retorno + Texto 
    }
    return( Retorno );
}
function Str2Dec( campo ) {
    campo = campo + "";
    campo = campo.replace( ".", "" );
    campo = campo.replace( ".", "" );
    campo = campo.replace( ".", "" );
    campo = campo.replace( ".", "" );
    campo = campo.replace( ".", "" );
    campo = campo.replace( ".", "" );
    campo = campo.replace( ".", "" );
    campo = campo.replace( ".", "" );
    campo = campo.replace( ",", "." );
    campo = parseFloat( campo );
    if( isNaN( campo ) ) {
	    return( 0 ); }
	else {
	    return( campo );
	}
}                   
function setVariables( x ){
	set_speed = 10;
	speed = ( x ) ? x : set_speed;
	ypos = 0;
	menu_w = 21;
	menu_h = 200;
	id_menu1= "atal"; 
	id_menu= id_menu1;
	doc_val1 = "";
	doc_val2 = "";
	style_val = ".style";
	h_val = ".pixelLeft";
	v_val = ".pixelTop";
	innerW = "document.body.clientWidth";
	innerH = "document.body.clientHeight";
	offsetX = "document.body.scrollLeft";
	offsetY = "document.body.scrollTop";
	checkLocation();
}
function checkLocation() {
	var availableX = eval( innerW );
	var availableY = eval( innerH );
	var currentX = eval( offsetX );
	var currentY = eval( offsetY );
	ydiff = ypos - currentY;
	if( ( ydiff < (-1 ) ) || ( ydiff > ( 1 ) ) ) movey = Math.round( ydiff / speed ), ypos -= movey;  
	mx = availableX - ( menu_w + 30 ) + currentX;
	my = availableY - ( menu_h + 20 ) + ypos;
	my = currentY + 1;
	evalMove();
	setTimeout( 'checkLocation()', 10 );
}
function evalMove(){
	x = doc_val1 + id_menu + doc_val2 + style_val + h_val + "=" + mx;
	y = doc_val1 + id_menu + doc_val2 + style_val + v_val + "=" + my;
	eval(y);
}
function FormataCampo( obj, cMask, xMasc ) {
	var strValor;
	if( event != null && ( event.keyCode >= 37 && event.keyCode <= 40 ) ){
		return false;
	}
	try{
		xMasc = xMasc.toUpperCase();
	}
	catch( e ){
		xMasc = '';
	}
	strValor = ObjVal( obj ) + '';
	if( xMasc == 'VALOR' ){
		return( ObjVal( obj, PreFormatacaoValor( strValor, cMask ) ) );
	}
	return( ObjVal( obj, VB_FormataCampo( strValor, cMask, xMasc ) ) );
}
function ObjVal( obj, atrbValor ){
	var retorno;
	if( obj.value == undefined ){
		retorno = obj;
		if( atrbValor != undefined ){
			retorno = atrbValor
		}
	}
	else {
		retorno = obj.value;
		if( atrbValor != undefined ){
			obj.value = atrbValor
		}
	}
	return( retorno );
}
function Chr( Valor ) {
	return VB_Chr( Valor );
}
function AbrirPesquisa( href_pesquisa, rotina, top, left, width, height, aEntrada, aSaida, scrollbars, status, toolbar, menubar, location, resizable, multiFrame ){
var 	strURL;
	topo = 220;
	esquerda = 166;
	largura = 618;
	altura = 260;
	screenParametros = "";
	ParametrosIN = "";
	ParametrosOUT = "";
	if( scrollbars != "yes" && scrollbars != "no" )
		scrollbars = "yes";
	if( status != "yes" && status != "no" )
		status = "no";
	if( toolbar != "yes" && toolbar != "no" )
		toolbar = "no";
	if( menubar != "yes" && menubar != "no" )
		menubar = "no";
	if( location != "yes" && location != "no" )
		location = "no";
	if( resizable != "yes" && resizable != "no" )
		resizable = "yes";
	screenParametros = "scrollbars=" + scrollbars + ", " + 
			   "status=" + status + ", " + 
			   "toolbar=" + toolbar + ", " + 
			   "menubar=" + menubar + ", " + 
			   "location=" + location + ", " + 
			   "resizable=" + resizable;
	if( top > 0 )
		topo = top;
	if( left > 0 )
		esquerda = left;
	if( width > 0 )
		largura = width;
	if( height > 0 )
		altura = height;
	for( Count = 0; Count < aEntrada.length; Count++ ) {
		cTMP = Count + 1;
		ParametrosIN = ParametrosIN + "cE" + cTMP + "=" + aEntrada[Count] + "&";
	}
	for( Count = 0; Count < aSaida.length; Count++ ) {
		cTMP = Count + 1;
		ParametrosOUT = ParametrosOUT + "cS" + cTMP + "=" + aSaida[Count] + "&";
	}
	if( multiFrame != undefined && multiFrame.toUpperCase() == 'MULTI-FRAME' ){
		if( ( ParametrosIN + ParametrosOUT ).length > 500 ){
			Mensagem( 'Excesso de par�metros na chamada', 16, 'Erro' );
		}
		strURL = href_pesquisa + '&call=' + rotina + '&parMultiFrame=' + replace( ParametrosIN + ParametrosOUT + 'SubSessionID=' + getNewSessionID(), '&', '�' );
	}
	else {
		strURL = href_pesquisa + '&call=' + rotina + '&SubSessionID=' + getNewSessionID() + "&" + ParametrosIN + ParametrosOUT;
	}
	try{
		var maxWin = aJanelas.length;
	}
	catch( e ){
		aJanelas = new Array();
	}
	aJanelas[aJanelas.length] = window.open( strURL, rotina, "top=" + topo + ", left=" + esquerda + ", width=" + largura + ", height=" + altura + ", " + screenParametros );
	aJanelas[aJanelas.length -1].focus();
	return false;
}
function DigitaCombo( objCombo ){
	var Key = event.keyCode;
	var Idx; 
	var aElem = new Array();
	aElem[32] = ' '; aElem[109] = '-'; aElem[188] = ','; aElem[189] = '-'; aElem[111] = '/'; aElem[190] = '.'; aElem[193] = '/'; aElem[110] = ','; aElem[194] = '.';
	try{
		Idx = sLetters;
	}
	catch( e ){
		sLetters = '';
		valAnterior = '';
	}
	if( sLetters.length == 0 && Key == 9 ){
		valAnterior = objCombo.selectedIndex;
	}
	if(! event.altKey ){
		if( Key == 8 ){
			sLetters = sLetters.substr( 0, sLetters.length - 1 );
		}
		if( Key == 46 ){
			sLetters = ''; 
			objCombo.selectedIndex = Idx;
			valAnterior = '';
					}
		if( Key == 13 || Key == 9  ){
			if( ( objCombo.onchange != null ) && ( valAnterior != objCombo.selectedIndex ) ){
				objCombo.onchange();
				valAnterior = objCombo.selectedIndex;
			}
			sLetters = ''; 
		}
		if( ( Key > 64 && Key < 91 ) || ( Key > 47 && Key < 58 ) || ( Key > 95 && Key < 106 ) || ( Key >= 109 && Key <= 193 ) || ( Key == 32 ) ){
			if( Key >= 96 && Key <= 105 ){
				sLetters = sLetters + ( Key - 96 );
			}
			else {
				if( ( Key >= 109 && Key <= 193 ) || Key == 32 ){
					sLetters = sLetters + aElem[Key];
				}
				else {
					sLetters = sLetters + String.fromCharCode(Key).toUpperCase();
				}
			}
			Idx = 0;
			for( var i=0; i < objCombo.options.length; i++ ){	
				if( objCombo.options[i].text.toUpperCase().substr(0,sLetters.length) == sLetters ){
					Idx = i; 
					break;
				}				   
			}
			objCombo.selectedIndex = Idx;
			event.returnValue = 0;
		}	
	}
}
function ValidaCampos( cPropriedade ) {
	var f = document.all;
	var isData = false;
	if( cPropriedade == undefined || cPropriedade == '' ){
		cPropriedade = 'obrigatorio';
	}
	for( var i=0; i < f.length; i++ ) {
		if( f[i].tagName == 'IMG' || f[i].style.display == 'none' )
			continue;
		isData = false;
                if( ( cPropriedade == 'obrigatorio' ) &&  
		    ( f[i].data == 'D' || f[i].data == 'M' || f[i].data == 'A' || f[i].data == 1 || f[i].data == 2 ) ){
			isData = true;
		}
		if( ( f[i][cPropriedade] == 1 && f[i].disabled == false ) || ( isData && f[i].value != '' ) ) {
			if( ! ValidaDMA( f[i].value, f[i].data ) ){
				f[i].select();
				return false;
			}
			if( f[i].data == 1 ){
				if( f[i].IniFim == 1 && ( f[i].value == '99/99/9999' || f[i].value == '00/00/0000' ) ){
					continue;
				}
				if( ! ValidaData( f[i].value ) ){
					f[i].select();
					return false;
				}
			}
			if( f[i].data == 2 ){
				if( ! ValidaData( '01/' + f[i].value ) ){
					f[i].select();
					return false;
				}
			}
			if( ( parseInt( replace( f[i].value, ',', '' ), 10 ) == 0 || f[i].value == '' ) && ! IsAlpha( f[i].value ) ) {
				alert( 'Informa��o inv�lida' );
				try{
					f[i].select();
				}
				catch( e ){
					f[i].focus();
				}
				return false;
			}
		}
	}
	return true;
}
function Rigth( texto, pos ){
	return( texto.substr( pos + 1, texto.length - pos - 1 ) );
}
function PreFormatacaoValor( valor, mascara ){
	valor += '';
	var mascaraDec, mascaraInt, valorDec, valorInt, posVirgula;
	var valorAux = '';
	while( true ){
		if( valor.substr(0,1) == ' ' || valor.substr(0,1) == '0' ){
			valor = valor.substr(1,valor.length - 1);
		}
		else{
			break;
		}
	}
	var jahTemPonto = false;
	while( true ){
		if( valor == '' ){
			break;
		}
		else{
			caracter = valor.substr(valor.length - 1, 1) + '';
			valor = valor.substr(0, valor.length - 1);
			if( jahTemPonto == true ){
				if( isNumeric(caracter) ){
					valorAux = caracter + valorAux;
				}
			}
			else{
				if( isNumeric(caracter) ){
					valorAux = caracter + valorAux;
				}
				else{
					if( caracter.charAt(0) == ',' || caracter.charAt(0) == '.' ){
						jahTemPonto = true;
						valorAux = ',' + valorAux;
					}
				}
			}
		}
	}
	valor = valorAux;
	posVirgula = mascara.search( ',' );
	if( posVirgula  == -1 ){
		mascaraDec = "";
		mascaraInt = mascara
	}
	else{
		mascaraInt = mascara.substr( 0, posVirgula );
		mascaraDec = Rigth(mascara, posVirgula);
	}
	posVirgula = valor.search( ',' );
	if( posVirgula  == -1 ){
		valorDec = "";
		valorInt = valor
	}
	else{
		valorInt = valor.substr( 0, posVirgula );
		valorDec = Rigth( valor, posVirgula );
	}
	var valorIntAux = '';
	if( mascaraInt == '' ){
		valorInt = '0';
	}
	else{
		while( true ){
			if( mascaraInt == "" ){
				break;
			}
			else{
				if( valorInt == '' ){
					break;
				}
				if( mascaraInt.substr( mascaraInt.length -1, 1 ) == '9' ){
					valorIntAux =  valorInt.substr(valorInt.length -1, 1 ) + valorIntAux ;
					valorInt = valorInt.substr(0, valorInt.length -1 );
				}
				else{
					if( mascaraInt.substr( mascaraInt.length -1, 1 ) == '.' ){
						valorIntAux = '.' + valorIntAux ;
					}
				}
				mascaraInt = mascaraInt.substr(0,mascaraInt.length -1);
			}
		}
	}
	if( valorIntAux == '' ){
		valorInt = '0';
	}
	else{
		valorInt = valorIntAux;
	}
	separador = ',';
	if( mascaraDec == '' ){
		valorDec = '';
		separador = '';
	}
	else{
		var valorDecAux = '';
		var tam = mascaraDec.length;
		for( i=0; i < tam ; i++){
			if( valorDec == '' ){
				valorDecAux = valorDecAux + '0';
			}
			else{
				valorDecAux = valorDecAux + valorDec.substr( 0, 1 );
				valorDec = Rigth( valorDec, 0 );
			}
		}
		valorDec = valorDecAux;
	}
	return valorInt + separador + valorDec ;
}
function SendForm( conjuntoObjs ){
    var strCampos = '';
    var f = document.forms[0];
    for( var i = 0; i < f.length; i++ ){
	if( f[i].type == 'text' ){
	    f[i].value = replace( f[i].value, '"', "'" );
	}
	if( ( f.elements[i].type == 'text' )      ||
	    ( f.elements[i].type == 'password' )  ||
	    ( f.elements[i].type == 'file' )      ||
	    ( f.elements[i].type == 'textarea' )  ||
	    ( f.elements[i].type == 'checkbox' )  ||
	    ( f.elements[i].type == 'radio' )     ||
	    ( f.elements[i].tagName == 'SELECT' ) ||
	    ( f.elements[i].type == 'hidden' ) ){
		if( ( f.elements[i].type == 'radio' ) || ( f.elements[i].type == 'checkbox' ) ){
	  	    if( f.elements[i].checked == true ){
			strCampos = strCampos +	'<input type=hidden name="' + f[i].name + '" value="' + f[i].value + '">';
		    }
		}
		else {
		    strCampos = strCampos +	'<input type=hidden name="' + f[i].name + '" value="' + f[i].value + '">';
		}
	    }
	}
    document.parentWindow.parent.bottom_rotina.document.all.ocultoSubmit.innerHTML = strCampos;
    document.parentWindow.parent.bottom_rotina.document.form1.submit();
}
function EnviarFormulario( conjuntoObjs ){
    if( document.parentWindow.parent.bottom_rotina.document.all.ocultoSubmit != undefined ){
   	  if( conjuntoObjs == undefined ){
				SendForm();
		  }else{
				SendFormArray( conjuntoObjs );
		  }
    }
    else{
        setTimeout( 'EnviarFormulario()', 200 );
    }
}
function SendFormArray( conjuntoObjs ){
	var strCampos = '';
	var f = document.forms[0];

	strCampos = strCampos +	'<input type=hidden name="SessionID"	 value="' + f('SessionID').value + '">';
	strCampos = strCampos +	'<input type=hidden name="SubSessionID" value="' + f('SubSessionID').value + '">';
	strCampos = strCampos +	'<input type=hidden name="opcao"			 value="' + f('opcao').value + '">';
	strCampos = strCampos +	'<input type=hidden name="ppopcao"		 value="' + f('ppopcao').value + '">';
	strCampos = strCampos +	'<input type=hidden name="call"			 value="' + f('call').value + '">';

	var ind = 0;
	while( ind < conjuntoObjs.length ){
   	if( ( f(conjuntoObjs[ind]).type == 'text' ) || 
			 ( f(conjuntoObjs[ind]).type == 'password' ) ||
			 ( f(conjuntoObjs[ind]).type == 'file' ) ||
			 ( f(conjuntoObjs[ind]).type == 'textarea' ) ||
			 ( f(conjuntoObjs[ind]).type == 'hidden' ) ||
			 ( f(conjuntoObjs[ind]).type == 'checkbox' ) ||
			 ( f(conjuntoObjs[ind]).tagName == 'SELECT' ) ){
			if( f(conjuntoObjs[ind]).type == 'checkbox' ){
				if( f(conjuntoObjs[ind]).checked == true ){
					var value = f(conjuntoObjs[ind]).value;
				}else{
					var value = 0;	
				}
				strCampos = strCampos +	'<input type=hidden name="' + conjuntoObjs[ind] + '" value="' + value + '">';
			}else{
				strCampos = strCampos +	'<input type=hidden name="' + conjuntoObjs[ind] + '" value="' + f(conjuntoObjs[ind]).value + '">';
			}
		}else{
			if( f(conjuntoObjs[ind])[0].type == 'radio' ){
				var ind1 = 0;
				while( ind1 < f(conjuntoObjs[ind]).length ){
					if( f(conjuntoObjs[ind])[ind1].checked == true ){
						strCampos = strCampos +	'<input type=hidden name="' + conjuntoObjs[ind] + '" value="' + f(conjuntoObjs[ind])[ind1].value + '">';
					}
					ind1++;
				}
			}
		}
		ind++;
	}
	document.parentWindow.parent.bottom_rotina.document.all.ocultoSubmit.innerHTML = strCampos;
	document.parentWindow.parent.bottom_rotina.document.form1.submit();
}  
function ComparaValor( obj1, obj2, strDesc ){
var val1 = parseInt( document.all[ obj1 ].value, 10 );
var val2 = parseInt( document.all[ obj2 ].value, 10 );
	if( isNaN( val1 ) ){ val1 = 0; }
	if( isNaN( val2 ) ){ val2 = 0; }
	if( val1 > val2 ){
		alert( strDesc + " inicial maior que final" );
		document.all[ obj1 ].select();
		return false;
	}
	return true;
}
function Disabled( cPropriedade, bStatus, aObjetos ) {
	var f = document.all;
	for( var i=0; i < f.length; i++ ) {
		if( aObjetos != undefined ){
			var ind = 0;
			while( ind < aObjetos.length ){
				if( ( f[i][aObjetos[ind]] == 1 && f[i].tagName != 'LABEL' ) ) {
					Disabilita( f[i], aObjetos[ind + 1] );
				}
				ind = ind +2 ;
			}	
		}else{
			if( ( f[i][cPropriedade] == 1 && f[i].tagName != 'LABEL' ) ) {
				Disabilita( f[i], bStatus );
			}
		}
	}
}
function Disabilita( objeto, bStatus ){
	if( objeto.type == 'text' ){
		if( bStatus ){
			objeto.className = 'clsDisabled';
		}
		else{
			objeto.className = 'campo';
		}
	}
	objeto.disabled = bStatus;
}
function DateDiff( dData1, dData2 ){
	return( VB_DateDiff( dData1, dData2 ) );
}
function Round( vlr, dec ){
	return( VB_Round( vlr, dec ) );
}