     ******************************************************************
      * Author: Julio Cesar
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. desafio02.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  RAD         PIC 9(2) VALUES ZEROS.
       01  INDICE      PIC 9(2) VALUES ZEROS.
       01  ENESIMO     PIC 9(2) VALUES ZEROS.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY 'Radicando'
           ACCEPT RAD.
           DISPLAY 'Indice'
           ACCEPT INDICE.

           COMPUTE ENESIMO =  RAD**(1/INDICE)
           DISPLAY 'Enesima raiz = ' ENESIMO.

            STOP RUN.
       END PROGRAM desafio02.
