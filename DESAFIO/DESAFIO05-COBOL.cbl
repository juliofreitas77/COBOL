      ******************************************************************
      * Author: GUSTAVO JACKSON PEREIRA
      * Date: 2019 11 11
      ******************************************************************
      *> 1  11-11-2019 *  CURSO        *
      *> 1             *  COBOL        *
      *> 1             *  DESAFIO05    *
      *>****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO05.
       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  WCA-PRINCIPAL    PIC X(20) VALUE 'PRINCIPAL'.
       01  WCA-INICIO       PIC X(20) VALUE 'INICIO'.
       01  WCA-PROCESSO     PIC X(20) VALUE 'PROCESSO'.
       01  WCA-FINALIZAR    PIC X(20) VALUE 'FINALIZAR'.

       01  WSN-IND       PIC 9(3) VALUE ZERO.
       01  WSN-BASE         PIC S9(3) VALUE ZERO.
       01  WSN-RES          COMP-2.
       01  WSN-TERM         COMP-2.
       77  WS-FIND          PIC 9(3) VALUE ZERO.
       77  WS-FRES          COMP-2.
       77  WS-FCAL          PIC 9(3) VALUE ZERO.

       PROCEDURE DIVISION.
      *>  MAIN-PROCEDURE.

       0000-PRINCIPAL.
           DISPLAY WCA-PRINCIPAL.
           PERFORM 1000-INICIO.
           PERFORM 3000-PROCESSO.
           PERFORM 9000-FINALIZAR.
           STOP RUN.
       0000-PRINCIPAL-FIM.

      *>      *----------------------------------------------------------------*
       1000-INICIO SECTION.
      *>      # COLETA DE INFORMACOES.
           DISPLAY WCA-INICIO
           DISPLAY 'INFORME A QUANTIDADE DE ELEMENTOS (N):'
           ACCEPT WSN-IND
           DISPLAY 'INFORME O VALOR DA BASE (X):'
           ACCEPT WSN-BASE
      *>      # DEFINE TERMO INICIAL
           MOVE 1 TO WSN-RES
      *>      # DEFINE EXPOENTE INICIAL (FATORIAL)
           MOVE 1 TO WS-FIND.
      *>      *----------------------------------------------------------------*

       3000-PROCESSO SECTION.
           IF  WSN-IND GREATER THAN 1
               PERFORM VARYING WS-FIND FROM 1 BY 1
               UNTIL WS-FIND EQUAL TO WSN-IND
                    PERFORM 4000-FACTORIAL
                     INITIALIZE  WSN-TERM
                     COMPUTE WSN-TERM = WSN-BASE ** WS-FIND
                     COMPUTE WSN-TERM = WSN-TERM / WS-FRES
                     COMPUTE WSN-RES = WSN-RES + WSN-TERM
                    END-PERFORM
           END-IF.
      *>      *----------------------------------------------------------------*

       4000-FACTORIAL SECTION.
      *>      # PARAMETRO DE ENTRADA: WS-FIND
           MOVE 1 TO WS-FCAL
           MOVE 1 TO WS-FRES
           PERFORM VARYING WS-FCAL FROM 1 BY 1
              UNTIL WS-FCAL GREATER THAN WS-FIND
              COMPUTE WS-FRES = WS-FRES * WS-FCAL
           END-PERFORM.

      *>      *----------------------------------------------------------------*
       9000-FINALIZAR SECTION.
               DISPLAY 'O VALOR FINAL DA SERIE EH: ' WSN-RES
               DISPLAY WCA-FINALIZAR.
      *>  *----------------------------------------------------------------*
       END PROGRAM DESAFIO05.
