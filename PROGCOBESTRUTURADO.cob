      ******************************************************************
      * Author: JULIO CESAR
      * Date: 25/09/2019
      * Purpose:Monte um programa que receba via console:
      *    Nome;
      *    Ano de entrada na empresa;
      *    Salário de um funcionário.
      *
      *    Para isso, aplique um aumento, de acordo com o tempo de
      *    casa baseado nas regras a seguir:
      *
      *    0 a 1 ano	0%
      *    de 2 a 05 anos	5%
      *    de 06 a 15 anos	10%
      *    a partir de 16 anos	15%

      *    NESTE CASO O CODIGO FOI ESTRUTURADO.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCICIOAULA07.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WRK-NOME         PIC X(50).
       77 WRK-ANO-ENTRADA  PIC 9(08).
       77 WRK-SALARIO      PIC 9(11)V99.
       77 WRK-SALARIO-MASC PIC $ZZZ.ZZZ,99.

       77 WRK-TEMPODECASA  PIC 9(02).

       01 WRK-DATA-SISTEMA.
          02 WRK-ANO   PIC 9(4).
          02 WRK-MES   PIC 9(2).
          02 WRK-DIA   PIC 9(2).
       PROCEDURE DIVISION.
       0001-PRINCIPAL.
           PERFORM 0100-INICIALIZAR.
           PERFORM 0200-PROCESSAR.
           PERFORM 0300-FINALIZAR.
           STOP RUN.
       0100-INICIALIZAR.
            DISPLAY 'NOME...'
            ACCEPT WRK-NOME FROM CONSOLE.
            DISPLAY 'ANO DE ENTRADA... '
            ACCEPT WRK-ANO-ENTRADA FROM CONSOLE
            DISPLAY 'SALARIO...'
            ACCEPT WRK-SALARIO FROM CONSOLE.

            ACCEPT WRK-DATA-SISTEMA FROM DATE YYYYMMDD.
       0200-PROCESSAR.
           COMPUTE WRK-TEMPODECASA = WRK-ANO - WRK-ANO-ENTRADA.
      *      COMPUTE WRK-SALARIO = WRK-SALARIO + (WRK-SALARIO * 0,05)
      *      MOVE WRK-SALARIO TO WRK-SALARIO-MASC.
      *      DISPLAY WRK-SALARIO-MASC.

            EVALUATE WRK-TEMPODECASA
            WHEN 2 THRU 5
               COMPUTE WRK-SALARIO = WRK-SALARIO + (WRK-SALARIO * 0,05)
               MOVE WRK-SALARIO TO WRK-SALARIO-MASC
            WHEN 6 THRU 15
               COMPUTE WRK-SALARIO = WRK-SALARIO + (WRK-SALARIO * 0,10)
               MOVE WRK-SALARIO TO WRK-SALARIO-MASC
            WHEN > 15
               COMPUTE  WRK-SALARIO = WRK-SALARIO + (WRK-SALARIO * 0,10).
               MOVE WRK-SALARIO TO WRK-SALARIO-MASC.


           DISPLAY 'TEMPO DE CASA ' WRK-TEMPODECASA ' ANO(S).'
           DISPLAY 'SALARIO COM REAJUSTE...' WRK-SALARIO-MASC.

       0300-FINALIZAR.
            DISPLAY'FIM DO PROCESSAMENTO.'.
       END PROGRAM EXERCICIOAULA07.
