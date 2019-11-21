      ******************************************************************
      * Author: DAVID MONTERO VIEIRA DOS SANTOS
      * Date: 13/11/219
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO12.
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

       01  WS-NUM           PIC 9(1).
       01  SUM-CPF          PIC 9(4) VALUE ZEROS.
       01  SUM-ADD          PIC 9(4) VALUE ZEROS.
       01  VAL-PRI          PIC 9(1).
       01  VAL-SEG          PIC 9(1).
       01  QUOCIENTE        PIC 9(2).
       01  RESTO            PIC 9(2).
       01  AUX              PIC 9(2).

       01  WSN-CPF.
           05  CPF-1            PIC 9(1).
           05  CPF-2            PIC 9(1).
           05  CPF-3            PIC 9(1).
           05  CPF-4            PIC 9(1).
           05  CPF-5            PIC 9(1).
           05  CPF-6            PIC 9(1).
           05  CPF-7            PIC 9(1).
           05  CPF-8            PIC 9(1).
           05  CPF-9            PIC 9(1).
           05  CPF-10           PIC 9(1).
           05  CPF-11           PIC 9(1).

       01  WSN-CNPJ.
           05  CNPJ-1            PIC 9(1).
           05  CNPJ-2            PIC 9(1).
           05  CNPJ-3            PIC 9(1).
           05  CNPJ-4            PIC 9(1).
           05  CNPJ-5            PIC 9(1).
           05  CNPJ-6            PIC 9(1).
           05  CNPJ-7            PIC 9(1).
           05  CNPJ-8            PIC 9(1).
           05  CNPJ-9            PIC 9(1).
           05  CNPJ-10           PIC 9(1).
           05  CNPJ-11           PIC 9(1).
           05  CNPJ-12           PIC 9(1).
           05  CNPJ-13           PIC 9(1).
           05  CNPJ-14           PIC 9(1).

       01  WSA-CAL OCCURS 12 TIMES.
           05 WSN-MES       PIC X(02).
           05 WSN-QTD       PIC 9(02).


       PROCEDURE DIVISION.

       0000-PRINCIPAL.
           DISPLAY WCA-PRINCIPAL.
           PERFORM 1000-INICIO.
           PERFORM 3000-PROCESSO.
           PERFORM 9000-FINALIZAR.
       0000-PRINCIPAL-FIM.


       1000-INICIO SECTION.
           DISPLAY 'DIGITE 1 PARA VALIDAR CPF'
                   'OU DIGITE 2 PARA VALIDAR CNPJ'
           ACCEPT WS-NUM
           EVALUATE WS-NUM
               WHEN 1
                   DISPLAY 'DIGITE O CPF'
               WHEN 2
                   DISPLAY 'DIGITE O CNPJ'
               WHEN OTHER
                   DISPLAY 'OPÇÃO INVALIDA'
                   PERFORM 9000-FINALIZAR
           END-EVALUATE
           ACCEPT WSN-CPF.

       3000-PROCESSO SECTION.
      ************* VALIDA CPF ************************************************
           IF WS-NUM EQUAL TO 1
               COMPUTE SUM-ADD = CPF-1 * 10
               ADD SUM-ADD TO SUM-CPF
               COMPUTE SUM-ADD = CPF-2 * 9
               ADD SUM-ADD TO SUM-CPF
               COMPUTE SUM-ADD = CPF-3 * 8
               ADD SUM-ADD TO SUM-CPF
               COMPUTE SUM-ADD = CPF-4 * 7
               ADD SUM-ADD TO SUM-CPF
               COMPUTE SUM-ADD = CPF-5 * 6
               ADD SUM-ADD TO SUM-CPF
               COMPUTE SUM-ADD = CPF-6 * 5
               ADD SUM-ADD TO SUM-CPF
               COMPUTE SUM-ADD = CPF-7 * 4
               ADD SUM-ADD TO SUM-CPF
               COMPUTE SUM-ADD = CPF-8 * 3
               ADD SUM-ADD TO SUM-CPF
               COMPUTE SUM-ADD = CPF-9 * 2
               ADD SUM-ADD TO SUM-CPF



               DIVIDE SUM-CPF BY 11  GIVING QUOCIENTE
                                          REMAINDER RESTO
               COMPUTE VAL-PRI = 11 - RESTO
      *         DISPLAY "PRIMEIRA CONTA" VAL-PRI

               IF VAL-PRI GREATER THAN 9
                   SET VAL-PRI TO 0
               END-IF
      *          DISPLAY "VAL PRI POS O IF" VAL-PRI

      *    VALIDA SEGUNDO DIGITO
               SET SUM-CPF TO 0
               COMPUTE SUM-ADD = CPF-1 * 11
               ADD SUM-ADD TO SUM-CPF
               COMPUTE SUM-ADD = CPF-2 * 10
               ADD SUM-ADD TO SUM-CPF
               COMPUTE SUM-ADD = CPF-3 * 9
               ADD SUM-ADD TO SUM-CPF
               COMPUTE SUM-ADD = CPF-4 * 8
               ADD SUM-ADD TO SUM-CPF
               COMPUTE SUM-ADD = CPF-5 * 7
               ADD SUM-ADD TO SUM-CPF
               COMPUTE SUM-ADD = CPF-6 * 6
               ADD SUM-ADD TO SUM-CPF
               COMPUTE SUM-ADD = CPF-7 * 5
               ADD SUM-ADD TO SUM-CPF
               COMPUTE SUM-ADD = CPF-8 * 4
               ADD SUM-ADD TO SUM-CPF
               COMPUTE SUM-ADD = CPF-9 * 3
               ADD SUM-ADD TO SUM-CPF
               COMPUTE SUM-ADD = CPF-10 * 2
               ADD SUM-ADD TO SUM-CPF

               display SUM-CPF

               DIVIDE SUM-CPF BY 11  GIVING QUOCIENTE
                                          REMAINDER RESTO

               COMPUTE VAL-SEG = 11 - RESTO

               IF VAL-SEG GREATER THAN 9
                   SET VAL-SEG TO 0
               END-IF

      *    VALIDAÇÃO FINAL CPF
      *     DISPLAY VAL-PRI ' ' CPF-10
               IF VAL-PRI EQUAL TO CPF-10 AND VAL-SEG EQUAL TO CPF-11
                       DISPLAY "CPF VALIDO"
                   ELSE
                       DISPLAY "CPF INVALIDO"
               END-IF
               ELSE
      ************* VALIDA CNPJ ***********************************************
                   IF WS-NUM EQUAL TO 2
                       COMPUTE SUM-ADD = CNPJ-1 * 5
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-2 * 4
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-3 * 3
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-4 * 2
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-5 * 9
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-6 * 8
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-7 * 7
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-8 * 6
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-9 * 5
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-10 * 4
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-11 * 3
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-12 * 2
                       ADD SUM-ADD TO SUM-CPF

                       DISPLAY 'SUM-CPF ' SUM-CPF

                       DIVIDE SUM-CPF BY 11  GIVING QUOCIENTE
                                                  REMAINDER RESTO
                       DISPLAY RESTO
                       COMPUTE VAL-PRI = 11 - RESTO
      *                 DISPLAY "PRIMEIRA CONTA" VAL-PRI

                       IF VAL-PRI LESS THAN 2
                           SET VAL-PRI TO 0
                       END-IF


      *    VALIDA SEGUNDO DIGITO
                       SET SUM-CPF TO 0
                       COMPUTE SUM-ADD = CNPJ-1 * 6
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-2 * 5
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-3 * 4
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-4 * 3
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-5 * 2
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-6 * 9
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-7 * 8
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-8 * 7
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-9 * 6
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-10 * 5
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-11 * 4
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-12 * 3
                       ADD SUM-ADD TO SUM-CPF
                       COMPUTE SUM-ADD = CNPJ-13 * 2
                       ADD SUM-ADD TO SUM-CPF

                       DIVIDE SUM-CPF BY 11 GIVING QUOCIENTE
                                                  REMAINDER RESTO
                       COMPUTE AUX = RESTO - 11

                       IF AUX LESS THAN 2
                           SET VAL-SEG TO 0
                           ELSE
                               COMPUTE VAL-SEG = 11 - AUX
                       END-IF

      *        VALIDAÇÃO FINAL CNPJ
                       IF VAL-PRI EQUAL TO CNPJ-13
                           AND VAL-SEG EQUAL TO CNPJ-14
                           DISPLAY "CNPJ VALIDO"
                       ELSE
                           DISPLAY "**** CNPJ INVALIDO ****"
                       END-IF
                   END-IF
               END-IF.


       9000-FINALIZAR SECTION.
               DISPLAY WCA-FINALIZAR
               STOP RUN.

       END PROGRAM DESAFIO12.
