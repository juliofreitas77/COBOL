       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGCOB04.
      ********************************************************
      *AREA DE COMENTARIOS - REMARKS
      *AUTHOR: Julio
      *OBJETIVO: RECEBER E IMPRIMIR FORMATADO - USO DA VIRGULA
      *          VARIAVEL MASCARA  E COMANDO MOVE PARA ATRIBUIR.
      ********************************************************
       ENVIRONMENT DIVISION.
      *CONFIGURANDO O SEPARADOR DE CASAS DECIMAIS COM VIRGULA.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WRK-NOME     PIC X(20) VALUE SPACES.
       77 WRK-SALARIO  PIC 9(06)V99 VALUE ZEROS.
      ************** VARIAVEL MASCARA
       77 WRK-SALARIO-ED PIC $ZZZ.ZZ9,99 VALUE ZEROS.

       PROCEDURE DIVISION.
      *****AREA DE COMANDOS 12 - 72
           ACCEPT WRK-NOME     FROM CONSOLE.
           ACCEPT WRK-SALARIO  FROM CONSOLE.
      ************************ MOSTRA DADOS
           DISPLAY 'NOME: ' WRK-NOME.
           MOVE WRK-SALARIO TO WRK-SALARIO-ED.
           DISPLAY 'SALARIO: ' WRK-SALARIO-ED.
           STOP RUN.
