      ******************************************************************
      * Author: Julio Cesar
      * Date: 25/09/2019
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCICIO01.
       ENVIRONMENT DIVISION.
      ******************************************************************
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      ******************************************************************
       INPUT-OUTPUT SECTION.
      *****************************************************************
       FILE-CONTROL.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       WORKING-STORAGE SECTION.
      *
       01 WCA-CONTANTES.
           05 WCA-PRINCIPAL  PIC X(10) VALUE 'PRINCIPAL'.
           05 WCA-INICIO     PIC X(10) VALUE 'INICIO'.
           05 WCA-PROCESSO   PIC X(10) VALUE 'PROCESSO'.
           05 WCA-FINALIZAR  PIC X(10) VALUE 'FINALIZAR'.
      *----------------------
       PROCEDURE DIVISION.
      *----------------------
       0000-PRINCIPAL.
            DISPLAY WCA-PRINCIPAL

            PERFORM 1000-INICIO
               THRU 1000-INICIO-FIM.

            PERFORM 3000-PROCESSO
               THRU 3000-PROCESSO-FIM.

            PERFORM 9000-FINALIZAR
               THRU 9000-FINALIZAR-FIM.

            STOP RUN.
      *-----------------------------------------------------------------
           1000-INICIO.
      *-----------------------------------------------------------------
            DISPLAY WCA-INICIO.
           1000-INICIO-FIM.

           3000-PROCESSO.
               DISPLAY WCA-PROCESSO.
           3000-PROCESSO-FIM.

           9000-FINALIZAR.
               DISPLAY WCA-FINALIZAR.
           9000-FINALIZAR-FIM.

       0000-PRINCIPAL-FIM.
           EXIT.
       END PROGRAM EXERCICIO01.
