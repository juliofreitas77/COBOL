      ******************************************************************
      * Author: JULIO CESAR
      * Date: 13/11/2019
      * Purpose:Criar um algoritmo que leia os valores: x e n
      * (n significa o enesimo termo desejado) e calcule a seguinte
      *  equação (Expansão de Taylor)
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO08.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  WRK-VARIAVEIS.
           05 X            PIC 9(02).
           05 N            PIC 9(02).
           05 CONTADORA    PIC 9(02).
           05 SOMA         PIC 9(02)       VALUE 1.
           05 AUX          PIC 9(02)       VALUE 1.
           05 AUX1         PIC 9(02)V9(9)  VALUES ZEROS.
           05 RESULT       COMP-2.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM 1000-INICIAR.
           PERFORM 2000-PRINCIPAL.
           PERFORM 3000-FINALIZAR.

           1000-INICIAR.
               DISPLAY 'DIGITE O VALOR DE X: '
               ACCEPT X
               IF X LESS THAN 1
                   DISPLAY 'DIGITE UM NUMERO VALIDO PARA X!'
                   PERFORM 1000-INICIAR
               ELSE
                   DISPLAY 'DIGITE O VALOR DE N: '
                   ACCEPT N
                   IF N LESS THAN 1
                       DISPLAY 'DIGITE UM NUMERO VALIDO PARA Y!'
                       PERFORM 1000-INICIAR
                   ELSE
                       CONTINUE
                   END-IF
               END-IF
                   .

            2000-PRINCIPAL.
                 PERFORM UNTIL CONTADORA EQUALS N

                 COMPUTE SOMA = SOMA * AUX
                 COMPUTE AUX1  = (X**AUX)/ SOMA
                 COMPUTE RESULT = RESULT + AUX1

                 ADD 1 TO CONTADORA
                 ADD 1 TO AUX
              END-PERFORM

           .
           3000-FINALIZAR.
               ADD 1 TO RESULT
               DISPLAY 'RESULTADO===> 'RESULT
               STOP RUN
           .
            STOP RUN.
       END PROGRAM DESAFIO08.
