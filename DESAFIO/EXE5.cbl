      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Exer5.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WCA-VARIAVEIS.
           05 X                 PIC 9(02) VALUES 1.
           05 N                 PIC 9(02) VALUE 1.
           05 X-AUX             PIC 9(02) VALUES 1.
           05 N-AUX             PIC 9(02) VALUES 1.
           05 AUX               PIC 9(02)V9(9) VALUES ZEROS.
           05 RESP              PIC 9(02)V9(9) VALUES ZEROS.
           05 AUX-COUNT         PIC 9(02) VALUES ZEROS.
           05 PARADA            PIC 9(02) VALUES ZEROS.
           05 CUMULATIVO        PIC 9(15) VALUE 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 1000-INICIO
           PERFORM 2000-FIM

           STOP RUN

           .
      *-----------------------------------------------------------------
       1000-INICIO.
      *-----------------------------------------------------------------

           DISPLAY "QUAL O VALOR DE X?"
           ACCEPT X

           DISPLAY "QUAL O VALOR DE N?"
           ACCEPT N

           SET PARADA TO N
           SET N-AUX TO 1

           PERFORM UNTIL AUX-COUNT = PARADA

           DISPLAY CUMULATIVO " " N-AUX

           COMPUTE CUMULATIVO = CUMULATIVO * N-AUX

           COMPUTE AUX = (X**N-AUX)/CUMULATIVO

           COMPUTE RESP = RESP + AUX

           ADD 1 TO AUX-COUNT
           ADD 1 TO X-AUX
           ADD 1 TO N
           ADD 1 TO N-AUX

           END-PERFORM
           .

      *-----------------------------------------------------------------
       2000-FIM.
      *-----------------------------------------------------------------

           ADD 1 TO RESP

           DISPLAY "O RESULTADO E: " RESP

           .
            STOP RUN.
       END PROGRAM Exer5.
