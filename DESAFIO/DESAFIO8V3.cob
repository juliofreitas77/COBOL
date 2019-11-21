      ******************************************************************
      * Author: Julio Cesar
      * Date: 18/11/2019
      * Purpose: IMPRESSAO DE NUMEROS PRIMOS PRESENTE NO INTEVALO DIGITA
      *    DO PER USUARIO, VALIDANDO TAMBEM SE O INTERVALO DIGITADO É
      *    VALIDO, SENDO O NROFIM MAIOR QUE O NROINI.
      * Tectonics: cobc
      * VERSAO: 3
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO08.
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
       1000-INICIO.

           DISPLAY "QUAL O NUMERO INICIAL?"
           ACCEPT NROINI


           IF NROINI GREATER THAN 1
               DISPLAY "QUAL O NUMERO FINAL?"
               ACCEPT NROFIM
               IF NROFIM GREATER THAN NROINI
                   CONTINUE
               ELSE
                   DISPLAY 'DIGITE UM INTEVALO VALIDO, OU SEJA, NUMERO '
                               'FINAL MAIOR QUE NUMERO INICIAL'
                   SET NROFIM TO 0
                   PERFORM 1000-INICIO
               END-IF

           ELSE
               DISPLAY 'DIGITE UM NUMERO MAIOR QUE 1 '
               SET NROINI TO 0
               PERFORM 1000-INICIO
           END-IF.

           SET AUX TO NROINI
           SET AUX2 TO NROINI

           PERFORM UNTIL AUX > NROFIM
               SET PERCORRER TO 2
               PERFORM UNTIL AUX2 = AUX + 1
                   DIVIDE AUX2  BY PERCORRER
                       GIVING RESULTADO REMAINDER RESTO
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
       2000-FIM.

            STOP RUN.
       END PROGRAM DESAFIO08.
