      $set preprocess (htmlpp) endp
      $set sourceformat"free"

      *>============================================================================================
       identification division.
       program-id. PROGRAMA01.
      *>============================================================================================

      *>
      *>            --------------------------------------------------------
      *>                               Programa Treinamento 01
      *>            --------------------------------------------------------
      *>

      *>============================================================================================
       environment division.
            special-names. decimal-point is comma.
      *>============================================================================================

            select arqindexado01 assign to disk wid-arqindexado01
                   organization    is indexed
                   access mode     is dynamic
                   record key      is reg-chave
                   lock mode       is manual
                   file status     is ws-resultado-acesso.

            select arqsequencial01 assign to disk wid-arqsequencial01
                   organization is line sequential
                   access mode is sequential
                   file status is ws-resultado-acesso.

      *>============================================================================================
       data division.

       *>a alteração que será feita no programa é :
           *> . tratar funcionário (não mais cliente)
           *> . se o funcionário tiver o reg-vl-salario-familia maior que zeros deverá
           *>   somar mais  R$ 120,00 no  reg-vl-salario-base
       *>CONCLUIDO


       fd  arqindexado01.

       01  reg-arq-indexado-01.
           03  reg-chave.
               05  reg-cd-funcionario    pic 9(07).
           03  reg-no-funcionario        pic x(30).
           03  reg-en-funcionario        pic x(30).
           03  reg-nu-funcionario        pic 9(05).
           03  reg-cp-funcionario        pic 9(09).
           03  reg-te-funcionario        pic 9(09).
           03  reg-vl-salario-base       pic 9(07)v99.
           03  reg-vl-salario-familia    pic 9(05)v99.
           03  reg-ec-funcionario        pic x(01).
           03  reg-uf-funcionario        pic x(02).
           03  reg-ci-funcionario        pic x(17).
           03  reg-ba-funcionario        pic x(17).

       fd  arqsequencial01.

       01  reg-arq-sequencial-01         pic x(150).

      *>============================================================================================
       working-storage section.
       78   versao                      value "a".

       01   ws-campos-de-trabalho.
            03 ws-resultado-acesso      pic x(2).
               88 ws-operacao-ok        value "00" "02".
               88 ws-arq-inexistente    value "35".

       01   cgi-input is external-form.
            03 f-campos-formulario.
               05 f-opcao                       pic 9(07) identified by "opcao".
               05 f-cd-funcionario              pic 9(07) identified by "cdFuncionario".
               05 f-no-funcionario              pic x(30) identified by "noFuncionario".
               05 f-en-funcionario              pic x(30) identified by "enFuncionario".
               05 f-nu-funcionario              pic 9(05) identified by "nuFuncionario".
               05 f-cp-funcionario              pic 9(09) identified by "cpFuncionario".
               05 f-te-funcionario              pic 9(09) identified by "teFuncionario".
               05 f-vl-salario-base             pic 9(07) identified by "vlSalarioBase".
               05 f-vl-salario-familia          pic 9(05) identified by "vlSalarioFamilia".
               05 f-ec-funcionario              pic x(01) identified by "ecFuncionario".
               05 f-uf-funcionario              pic x(02) identified by "ufFuncionario".
               05 f-ci-funcionario              pic x(17) identified by "ciFuncionario".
               05 f-ba-funcionario              pic x(17) identified by "baFuncionario".

      *>============================================================================================
       procedure division.

      *>============================================================================================
       0000-controle section.
       0000.
            perform 1000-inicializacao
            perform 2000-processamento
            perform 3000-finalizacao.
       0000-saida.
            exit program
            stop run.

      *>============================================================================================
       1000-inicializacao section.
       1000.
            move spaces                 to cgi-input
            initialize                  cgi-input
            accept cgi-input

            perform 1100-cabecalho-html-aux
            .

       1000-exit.
            exit.

      *>============================================================================================
       1100-cabecalho-html section.
       1100.
            exec html
<html>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<HEAD>
<meta http-equiv="Expires" content="Mon, 01 Jan 1996 12:12:00 GMT">
<meta http-equiv="Pragma" content="no-cache">
<META content="text/html; charset=windows-1252" http-equiv=Content-Type>
<HEAD> <TITLE>PROGRAMA 01</TITLE> </HEAD>
<link rel="stylesheet" href="acao.css" type="text/css"></link>
<script src="funcoes.js" language="JavaScript"></script>
<script src="funcoes.vbs" language="VbScript"></script>
<BODY leftMargin=0 topMargin=0 marginheight="0" marginwidth="0">
<FORM name=form1 method=post>
            end-exec.
       1100-exit.
            exit.

      *>============================================================================================
       1100-cabecalho-html-aux section.
       1100.
            exec html
<html>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<HEAD>
<meta http-equiv="Expires" content="Mon, 01 Jan 1996 12:12:00 GMT">
<meta http-equiv="Pragma" content="no-cache">
<META content="text/html; charset=windows-1252" http-equiv=Content-Type>
<HEAD> <TITLE>PROGRAMA 01</TITLE> </HEAD>
<script src="funcoes.js" language="JavaScript"></script>
<script src="funcoes.vbs" language="VbScript"></script>
<BODY leftMargin=0 topMargin=0 marginheight="0" marginwidth="0">
<FORM name=form1 method=post>
            end-exec.
       1100-exit.
            exit.


      *>============================================================================================
       2000-processamento section.
       2000.
            evaluate f-opcao
                when 0
                    perform 2100-carrega-tela
                when 2
                    perform 2200-salva
                when 3
                    perform 2300-exclui
                when 4
                    perform 2400-carrega
                when 5
                    perform 2500-salario-familia
                when 6
                    perform 2600-modifica
                when 7
                    perform 2700-limpa
                when 8
                   perform 2910-anterior
                when 9
                   perform 2920-proximo
                when other
                    exec html
                       <script>
                           alert( 'Opção inválida' );
                       </script>
                    end-exec
            end-evaluate.
       2000-exit.
            exit.

      *>============================================================================================
       2100-carrega-tela section.
       2100.
            perform 8000-tela.
       2100-exit.
            exit.
      *>============================================================================================
       2200-salva section.
       2200.
            perform 2900-move
            perform 2800-abre
            if   ws-operacao-ok
                 write reg-arq-indexado-01
                 exec html
                    <script>
                        alert( 'Gravou :f-cd-funcionario' );
                    </script>
                 end-exec
                 move "C:\Treinamento\Treinamento02\LOG.TXT" to wid-arqsequencial01
                 open extend arqsequencial01
                 move spaces to reg-arq-sequencial-01
                 string "Gravou "
                        f-cd-funcionario ";"
                        f-no-funcionario delimited by "   " ";"
                        f-en-funcionario delimited by "   " ";"
                        f-nu-funcionario delimited by "   " ";"
                        f-cp-funcionario ";"
                        f-te-funcionario ";"
                        f-vl-salario-base ";"
                        f-vl-salario-familia ";"
                        f-ec-funcionario ";"
                        f-uf-funcionario ";"
                        f-ci-funcionario delimited by "   " ";"
                        f-ba-funcionario delimited by "   " ";" into reg-arq-sequencial-01
                 write reg-arq-sequencial-01
            end-if
            perform 8000-tela.
       2200-exit.
            exit.

      *>============================================================================================
       2300-exclui section.
       2300.
            perform 2800-abre
            perform 2900-move
            read arqindexado01
            delete arqindexado01
                 exec html
                    <script>
                        alert( 'Funcionário Excluido' );
                    </script>
                 end-exec.
               perform 8000-tela.
       2300-exit.
            exit.

      *>============================================================================================
       2400-carrega section.
       2400.
            perform 2800-abre
            perform 2900-move
            read arqindexado01
            if   ws-operacao-ok
                 exec html
                    <script>
                        alert( 'Funcionário já cadastrado' );
                    </script>
                 end-exec
                 perform 8000-tela
                 perform 2990-localiza
             else
                 perform 8000-tela
                 exec html
                    <script>
                        alert( 'Funcionário não encontrado' );
                        document.all.cdFuncionario.value = ":reg-cd-funcionario";
                    </script>
                 end-exec
            end-if
            perform 2992-abilita-desabilita
            if   f-cd-funcionario = 1
                 exec html
                     <script>
                         document.all.BotProximo.disabled = false;
                         document.all.BotAnterior.disabled = true;
                     </script>
                 end-exec
            end-if
            read arqindexado01 next
            if   not ws-operacao-ok
                 exec html
                      <script>
                          document.all.BotAnterior.disabled = false;
                          document.all.BotProximo.disabled = true;
                      </script>
                 end-exec.
       2400-exit.
            exit.

      *>============================================================================================
       2500-salario-familia section.
       2500.
            if   f-vl-salario-familia  is greater then 0
                 ADD 120,00 to f-vl-salario-base
                 exec html
                   <script>
                       alert( 'Foi adicionado R$ 120,00 Reais ao Salário Base' );
                   </script>
                 end-exec
            else
               exec html
                   <script>
                       alert( 'Salário Base não modificado!' );
                   </script>
               end-exec
            end-if
            perform 8000-tela
            perform 2991-localiza-salario.
            perform 2992-abilita-desabilita.
       2500-exit.
            exit.

      *>============================================================================================
       2600-modifica section.
       2600.
            perform 2900-move
            perform 2800-abre
            if   ws-operacao-ok
                 rewrite reg-arq-indexado-01
                 exec html
                    <script>
                        alert( 'Modificou :f-cd-funcionario' );
                    </script>
                 end-exec
                 move "C:\Treinamento\Treinamento02\LOG.TXT" to wid-arqsequencial01
                 open extend arqsequencial01
                 move spaces to reg-arq-sequencial-01
                 string "Modificou "
                        f-cd-funcionario ";"
                        f-no-funcionario delimited by "   " ";"
                        f-en-funcionario delimited by "   " ";"
                        f-nu-funcionario delimited by "   " ";"
                        f-cp-funcionario ";"
                        f-te-funcionario ";"
                        f-vl-salario-base ";"
                        f-vl-salario-familia ";"
                        f-ec-funcionario ";"
                        f-uf-funcionario ";"
                        f-ci-funcionario delimited by "   " ";"
                        f-ba-funcionario delimited by "   " ";" into reg-arq-sequencial-01
                 write reg-arq-sequencial-01
            end-if
            perform 8000-tela.
       2600-exit.
            exit.

      *>============================================================================================
       2700-limpa section.
       2700.
            perform 2800-abre
            perform 2900-move
            read arqindexado01
            cancel "arqindexado01"
            perform 8000-tela.
       2700-exit.
            exit.

      *>============================================================================================
       2800-abre section.
       2800.
            move "C:\FUNCION.CAD" to wid-arqindexado01
            open i-o arqindexado01
            if   not ws-operacao-ok
                 exec html
                    <script>
                        alert( 'Erro :ws-resultado-acesso' );
                    </script>
                 end-exec
                 exit section
            end-if.
       2800-exit.
            exit.

      *>============================================================================================
       2900-move section.
       2900.
            initialize reg-arq-indexado-01
            move f-cd-funcionario           to reg-cd-funcionario
            move f-no-funcionario           to reg-no-funcionario
            move f-nu-funcionario           to reg-nu-funcionario
            move f-cp-funcionario           to reg-cp-funcionario
            move f-te-funcionario           to reg-te-funcionario
            move f-en-funcionario           to reg-en-funcionario
            move f-vl-salario-base          to reg-vl-salario-base
            move f-vl-salario-familia       to reg-vl-salario-familia
            move f-ec-funcionario           to reg-ec-funcionario
            move f-uf-funcionario           to reg-uf-funcionario
            move f-ci-funcionario           to reg-ci-funcionario
            move f-ba-funcionario           to reg-ba-funcionario.
       2900-exit.
            exit.

      *>============================================================================================
       2990-localiza section.
       2990.
            exec html
               <script>
                  document.all.cdFuncionario.value =    ":reg-cd-funcionario";
                  document.all.noFuncionario.value =    ":reg-no-funcionario";
                  document.all.enFuncionario.value =    ":reg-en-funcionario";
                  document.all.nuFuncionario.value =    ":reg-nu-funcionario";
                  document.all.cpFuncionario.value =    ":reg-cp-funcionario";
                  document.all.teFuncionario.value =    ":reg-te-funcionario";
                  document.all.vlSalarioBase.value =    ":reg-vl-salario-base";
                  document.all.vlSalarioFamilia.value = ":reg-vl-salario-familia";
                  document.all.ecFuncionario.value =    ":reg-ec-funcionario";
                  document.all.ufFuncionario.value =    ":reg-uf-funcionario";
                  document.all.ciFuncionario.value =    ":reg-ci-funcionario";
                  document.all.baFuncionario.value =    ":reg-ba-funcionario";
               </script>
            end-exec.
       2990-exit.
            exit.

      *>============================================================================================
       2991-localiza-salario section.
       2991.
            exec html
               <script>
                  document.all.cdFuncionario.value =    ":f-cd-funcionario";
                  document.all.noFuncionario.value =    ":f-no-funcionario";
                  document.all.enFuncionario.value =    ":f-en-funcionario";
                  document.all.nuFuncionario.value =    ":f-nu-funcionario";
                  document.all.cpFuncionario.value =    ":f-cp-funcionario";
                  document.all.teFuncionario.value =    ":f-te-funcionario";
                  document.all.vlSalarioBase.value =    ":f-vl-salario-base";
                  document.all.vlSalarioFamilia.value = ":f-vl-salario-familia";
                  document.all.ecFuncionario.value =    ":f-ec-funcionario";
                  document.all.ufFuncionario.value =    ":f-uf-funcionario";
                  document.all.ciFuncionario.value =    ":f-ci-funcionario";
                  document.all.baFuncionario.value =    ":f-ba-funcionario";
               </script>
            end-exec.
       2991-exit.
            exit.

      *>============================================================================================
       2992-abilita-desabilita section.
       2992.
            if   ws-operacao-ok
                 exec html
                   <script>
                       Disabled( 'bloco', true);
                       document.all.BotAnterior.disabled = false;
                       document.all.BotProximo.disabled = false;
                   </script>
                end-exec.
       2992-exit.
            exit.

      *>============================================================================================
       2910-anterior section.
       2910.
            perform 2800-abre
            initialize reg-arq-indexado-01
            move f-cd-funcionario      to reg-cd-funcionario
            start arqindexado01 key is less than reg-arq-indexado-01
            read arqindexado01 previous
            perform 8000-tela
            perform 2992-abilita-desabilita
            perform 2990-localiza
            read arqindexado01 previous
            if   not ws-operacao-ok
                 exec html
                     <script>
                         document.all.BotAnterior.disabled = true;
                         document.all.BotProximo.disabled = false;
                     </script>
                 end-exec
            else
                 exec html
                     <script>
                         document.all.BotAnterior.disabled = false;
                         document.all.BotProximo.disabled = false;
                     </script>
                  end-exec
            end-if.
       2910-exit.
            exit.

      *>============================================================================================
       2920-proximo section.
       2920.
            perform 2800-abre
            initialize reg-arq-indexado-01
            move f-cd-funcionario      to reg-cd-funcionario
            start arqindexado01 key is greater than reg-arq-indexado-01
            read arqindexado01 next
            perform 8000-tela
            perform 2992-abilita-desabilita
            perform 2990-localiza
            read arqindexado01 next
            if   not ws-operacao-ok
                 exec html
                     <script>
                        document.all.BotProximo.disabled = true;
                        document.all.BotAnterior.disabled = false;
                    </script>
                 end-exec
            else
                 exec html
                     <script>
                         document.all.BotProximo.disabled = false;
                         document.all.BotAnterior.disabled = false;
                     </script>
                 end-exec
            end-if.
       2920-exit.
            exit.

      *>============================================================================================
       3000-finalizacao section.
       3000.
            close arqindexado01 arqsequencial01.
       3000-exit.
            exit.

      *>============================================================================================
       8000-tela section.
       8000.
            exec html
            <!--STYLE>

            .titulocampo
            {
                FONT-WEIGHT: bolder;
                FONT-SIZE: 8pt;
                COLOR: navy;
                FONT-FAMILY: Arial;
                TEXT-ALIGN: right
            }
            </STYLE-->
               <INPUT type=hidden name=opcao>
               <TABLE width=100%>
                  <TR>
                     <TD class=titulocampo><CENTER>CADASTRO DE FUNCIONARIOS</CENTER><TR>
                     <TD>
                        <TABLE rules=none width=100% border=1 cellspacing=1 cellpadding=0 bordercolor=White bordercolordark=White bordercolorlight=DimGray class=WindowScreen>
                           <TR>
                              <TD colspan=4>&nbsp;</TD>
                           <TR>
                              <TD width=20% class=titulocampo>Código&nbsp;</TD>
                              <TD colspan=3><INPUT class="campo" bloco=1 name=cdFuncionario tabIndex=1 size=07 maxlength=7 onChange="Carregar()" onKeyPress="return SomenteNumeros();"></TD>
                           <TR>
                              <TD class=titulocampo><B>Nome&nbsp;</TD>
                              <TD colspan=3><INPUT class="campo" bloco=1 name=noFuncionario tabIndex=2 size=40 maxlength=40 ></TD>
                           <TR>
                              <TD class=titulocampo>CPF&nbsp;</TD>
                              <TD><INPUT class="campo" bloco=1 name=cpFuncionario tabIndex=3 size=9 maxlength=9 onKeyPress="return SomenteNumeros();" ></TD>

                              <TD class=titulocampo>Estado Civil&nbsp;</TD>
                              <TD><SELECT class="campo" bloco=1 name=ecFuncionario tabIndex=4 obrigatorio=1 >
                              <OPTION value="">Selecionar</OPTIONS>
                              <OPTION value="S">Solteiro(a)</OPTIONS>
                              <OPTION value="C">Casado(a)</OPTIONS>
                           <TR>
                              <TD class=titulocampo>Endereço&nbsp;</TD>
                              <TD><INPUT class="campo" bloco=1 name=enFuncionario tabIndex=5 size=40 maxlength=40 ></TD>

                              <TD class=titulocampo>Nº&nbsp;</TD>
                              <TD><INPUT class="campo" bloco=1 name=nuFuncionario tabIndex=6 size=05 maxlength=05 onKeyPress="return SomenteNumeros();" ></TD>
                           <TR>
                              <TD class=titulocampo>Bairro&nbsp;</TD>
                              <TD colspan=3><INPUT class="campo" bloco=1 name=baFuncionario tabIndex=7 size=20 maxlength=20 ></TD>
                           <TR>
                              <TD class=titulocampo>Cidade&nbsp;</TD>
                              <TD><INPUT class="campo" bloco=1 name=ciFuncionario tabIndex=8 size=20 maxlength=20 ></TD>

                              <TD class=titulocampo>UF&nbsp;</TD>
                              <TD><SELECT class="campo" bloco=1 name=ufFuncionario tabIndex=9 obrigatorio=1 >
                              <OPTION value="">Selecionar</OPTIONS>
                              <OPTION value="SC">SC</OPTIONS>
                              <OPTION value="SP">SP</OPTIONS>
                           <TR>
                              <TD class=titulocampo>Telefone&nbsp;</TD>
                              <TD colspan=3><INPUT class="campo" bloco=1 name=teFuncionario tabIndex=10 size=9 maxlength=9 onKeyPress="return SomenteNumeros();" ></TD>
                           <TR>
                              <TD class=titulocampo>Salário Base&nbsp;</TD>
                              <TD colspan=3><INPUT class="campo" bloco=1 name=vlSalarioBase tabIndex=11 size=9 maxlength=9 ></TD>
                           <TR>
                              <TD class=titulocampo>Salário Família&nbsp;</TD>
                              <TD colspan=3><INPUT class="campo" bloco=1 name=vlSalarioFamilia tabIndex=12 size=9 maxlength=9 onChange="SalarioFamilia()" ></TD>
                           <TR>
                              <TD colspan=4>&nbsp;</TD>
                         </TABLE></TD>
                 <TR>
                    <TD>
                        <TABLE border=0 width=100%>
                           <TR>
                              <TD width=0%>&nbsp;
                              <TD><BUTTON name=BotAnterior AccessKEY="A" tabIndex=504 onClick="Anterior();" DISABLED ><LABEL><U>A</U>nterior</LABEL></BUTTON></TD>
                              <TD><BUTTON name=BotProximo AccessKEY="P" tabIndex=505 onClick="Proximo();" DISABLED ><LABEL><U>P</U>róximo</LABEL></BUTTON></TD>
                              <TD width=100%>&nbsp;
                              <TD><BUTTON name=BotSalvar AccessKEY="S" tabIndex=500 onClick="Salvar();"><LABEL><U>S</U>alvar</LABEL></BUTTON></TD>
                              <TD><BUTTON name=BotModificar AccessKEY="M" tabIndex=501 onClick="Modificar();"><LABEL><U>M</U>odificar</LABEL></BUTTON></TD>
                              <TD><BUTTON name=BotLimpar AccessKEY="L" tabIndex=502 onClick="Limpar();"><LABEL><U>L</U>impar</LABEL></BUTTON></TD>
                              <TD><BUTTON name=BotExcluir AccessKEY="E" tabIndex=503 onClick="Excluir();"><LABEL><U>E</U>xcluir</LABEL></BUTTON></TD>
                        </TABLE></TD>
                 </TABLE>
            end-exec

            exec html
               <SCRIPT>
                   function Salvar(){
                       document.all.cdFuncionario.disabled = false;
                       document.all.opcao.value = 2;
                       document.form1.submit();
                   }
                   function Excluir(){
                       document.all.cdFuncionario.disabled = false;
                       document.all.opcao.value = 3;
                       document.form1.submit();
                   }
                   function Carregar(){
                       document.all.cdFuncionario.disabled = false;
                       document.all.opcao.value = 4;
                       document.form1.submit();
                   }
                   function SalarioFamilia(){
                       document.all.cdFuncionario.disabled = false;
                       document.all.opcao.value = 5;
                       document.form1.submit();
                   }
                   function Modificar(){
                       document.all.cdFuncionario.disabled = false;
                       document.all.opcao.value = 6;
                       document.form1.submit();
                   }
                   function Limpar(){
                       document.all.cdFuncionario.disabled = false;
                       document.all.opcao.value = 7;
                       document.form1.submit();
                   }
                   function Anterior(){
                       document.all.cdFuncionario.disabled = false;
                       document.all.opcao.value = 8;
                       document.form1.submit();
                   }
                   function Proximo(){
                       document.all.cdFuncionario.disabled = false;
                       document.all.opcao.value = 9;
                       document.form1.submit();
                   }

               </SCRIPT>
            end-exec.
       8000-exit.
            exit.

      *>
