       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGCOB05.
      ********************************************************
      *AREA DE COMENTARIOS - REMARKS
      *AUTHOR: Julio
      *OBJETIVO: TESTAR OS OPERADORES ARITMETICOS
      *DATA:
      ********************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WRK-NUM1     PIC 9(02) VALUE ZEROS.
       77 WRK-NUM2     PIC 9(02) VALUE ZEROS.
       77 WRK-RESUL    PIC 9(04) VALUE ZEROS.
       77 WRK-RESTO    PIC 9(04) VALUE ZEROS.
       PROCEDURE DIVISION.
      *****AREA DE COMANDOS 12 - 72
           ACCEPT WRK-NUM1 FROM CONSOLE.
           ACCEPT WRK-NUM2 FROM CONSOLE.
           DISPLAY 'NUMERO 1...' WRK-NUM1.
           DISPLAY 'NUMERO 2...' WRK-NUM2.

      *****SOMA
           ADD WRK-NUM1 WRK-NUM2 TO WRK-RESUL.
           DISPLAY 'RESULTADO DA SOMA... ' WRK-RESUL.
      *****SUBTRA��O
           SUBTRACT WRK-NUM1 FROM WRK-NUM2 GIVING WRK-RESUL.
           DISPLAY 'RESULTADO DA SUBTRACAO...' WRK-RESUL.
      *****DIVIS�O COM OU SEM RESTO NO REMAINDER
           DIVIDE WRK-NUM1 BY WRK-NUM2 GIVING WRK-RESUL
             REMAINDER WRK-RESTO.
           DISPLAY'RESULTADO DA DIVISAO...' WRK-RESUL.
           DISPLAY 'RESTO DA DIVISAO...' WRK-RESTO.
      *****MULTIPLICACAO
           MULTIPLY WRK-NUM1 BY WRK-NUM2 GIVING WRK-RESUL.
           DISPLAY 'RESULTADO DA MULTIPLICACAO...' WRK-RESUL.

      *****COMPUTE ***********
           COMPUTE WRK-RESUL = (WRK-NUM1 + WRK-NUM2) /2.
           DISPLAY 'Media utilizando COMPUTE... 'WRK-RESUL.
           STOP RUN.
