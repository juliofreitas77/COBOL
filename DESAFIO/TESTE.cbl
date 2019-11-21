      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Exer8.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WCA-VARIAVEIS.
           05 NROINI            PIC 9(02) VALUES ZEROS.
           05 NROFIM            PIC 9(02) VALUES ZEROS.
           05 QTDPRIMOS         PIC 9(02) VALUES ZEROS.
           05 PERCORRER         PIC 9(02) VALUES ZEROS.
           05 AUX               PIC 9(02) VALUE 1.
           05 AUX2              PIC 9(02) VALUE 1.
           05 RESTO             PIC 9(02) VALUE 1.
           05 RESULTADO         PIC 9(02) VALUE 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 1000-INICIO
           PERFORM 2000-FIM

           STOP RUN

           .
      *------------------------------------------------------------------------
       1000-INICIO.
      *------------------------------------------------------------------------

           DISPLAY "QUAL O NUMERO INICIAL?"
           ACCEPT NROINI

           DISPLAY "QUAL O NUMERO FINAL?"
           ACCEPT NROFIM

           SET AUX TO NROINI
           SET AUX2 TO NROINI



           PERFORM UNTIL AUX EQUAL NROFIM + 1

           SET PERCORRER TO 2

           PERFORM UNTIL AUX2 = AUX  + 1

           DIVIDE AUX2  BY PERCORRER GIVING RESULTADO REMAINDER RESTO


           IF RESTO NOT EQUALS 0 OR AUX2 EQUAL 2
               DISPLAY AUX2 " " WITH NO ADVANCING
               ADD 1 TO QTDPRIMOS
           END-IF

           ADD 1 TO AUX2
           ADD 1 TO PERCORRER

           END-PERFORM


           ADD 1 TO AUX

           END-PERFORM



           .

      *------------------------------------------------------------------------
       2000-FIM.
      *------------------------------------------------------------------------



           DISPLAY "A QUANTIDADE NUMEROS PRIMOS ENTRE " NROINI " E "
           WITH NO ADVANCING
           DISPLAY NROFIM " E DE: " QTDPRIMOS

           .
            STOP RUN.
       END PROGRAM Exer8.
