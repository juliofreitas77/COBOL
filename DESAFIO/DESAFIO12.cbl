      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.

       FILE SECTION.
       WORKING-STORAGE SECTION.
       1  WCA-VARIAVEIS.
           05 SELECAO PIC 9(1) VALUE ZERO.
           05 CPF.
               10 D1                      PIC 9(1).
               10 D2                      PIC 9(1).
               10 D3                      PIC 9(1).
               10 D4                      PIC 9(1).
               10 D5                      PIC 9.
               10 D6                      PIC 9.
               10 D7                      PIC 9.
               10 D8                      PIC 9.
               10 D9                      PIC 9.
               10 D10                     PIC 9.
               10 D11                     PIC 9.
           05 AUX                         PIC 9(09) VALUE ZERO.
           05 AUX2                        PIC 9(09) VALUE ZERO.
           05 AUX3                        PIC 9(09) VALUE ZERO.
           05 QUOCIENTE                   PIC 9(09).
           05 RESTO                       PIC 9(09).
           05 VALIDA-D1                   PIC 9(02).
           05 VALIDA-D2                   PIC 9(02).
           05 CNPJ.
               10 DPJ1                    PIC 9.
               10 DPJ2                    PIC 9.
               10 DPJ3                    PIC 9.
               10 DPJ4                    PIC 9.
               10 DPJ5                    PIC 9.
               10 DPJ6                    PIC 9.
               10 DPJ7                    PIC 9.
               10 DPJ8                    PIC 9.
               10 DPJ9                    PIC 9.
               10 DPJ10                   PIC 9.
               10 DPJ11                   PIC 9.
               10 DPJ12                   PIC 9.
               10 DPJ13                   PIC 9.
               10 DPJ14                   PIC 9.
               10 DPJ15                   PIC 9.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESSA
           PERFORM 3000-FIM

            STOP RUN.
           1000-INICIO.
              DISPLAY '1 PARA CPF E 2 PARA VALIDAR CNPJ'
              ACCEPT SELECAO
           .
           2000-PROCESSA.
              EVALUATE SELECAO
               WHEN 1
                   PERFORM 2100-VALIDA-CPF
               WHEN 2
                   PERFORM 2200-VALIDA-CNPJ
               WHEN OTHER
                   DISPLAY 'DIGITE UMA OPCAO VALIDA!'
                   PERFORM 3000-FIM
               END-EVALUATE
           .
           2100-VALIDA-CPF.
               DISPLAY 'DIGITE O CPF: '
               ACCEPT CPF
               COMPUTE AUX = D1 * 10
                       ADD AUX TO AUX2
               COMPUTE AUX = D2 * 9
                       ADD AUX TO AUX2
               COMPUTE AUX = D3 * 8
                       ADD AUX TO AUX2
               COMPUTE AUX = D4 * 7
                       ADD AUX TO AUX2
               COMPUTE AUX = D5 * 6
                       ADD AUX TO AUX2
               COMPUTE AUX = D6 * 5
                       ADD AUX TO AUX2
               COMPUTE AUX = D7 * 4
                       ADD AUX TO AUX2
               COMPUTE AUX = D8 * 3
                       ADD AUX TO AUX2
               COMPUTE AUX = D9 * 2
                       ADD AUX TO AUX2

               DIVIDE AUX2 BY 11 GIVING QUOCIENTE  REMAINDER RESTO

               COMPUTE VALIDA-D1 = 11 - RESTO
               IF VALIDA-D1 GREATER THAN 9
                   SET VALIDA-D1 TO 0
               END-IF
      *    SEGUNDA VALIDAÇÃO

               SET AUX2 TO 0
               COMPUTE AUX = D1 * 11
                       ADD AUX TO AUX2
               COMPUTE AUX = D2 * 10
                       ADD AUX TO AUX2
               COMPUTE AUX = D3 * 9
                       ADD AUX TO AUX2
               COMPUTE AUX = D4 * 8
                       ADD AUX TO AUX2
               COMPUTE AUX = D5 * 7
                       ADD AUX TO AUX2
               COMPUTE AUX = D6 * 6
                       ADD AUX TO AUX2
               COMPUTE AUX = D7 * 5
                       ADD AUX TO AUX2
               COMPUTE AUX = D8 * 4
                       ADD AUX TO AUX2
               COMPUTE AUX = D9 * 3
                       ADD AUX TO AUX2
               COMPUTE AUX = D10 * 2
                       ADD AUX TO AUX2

               DIVIDE AUX2 BY 11 GIVING QUOCIENTE REMAINDER RESTO
               COMPUTE VALIDA-D2 = 11 - RESTO
               IF VALIDA-D2 GREATER THAN 9
                   SET VALIDA-D2 TO 0
               END-IF

           IF VALIDA-D1 EQUAL TO D10 AND VALIDA-D2 EQUAL TO D11
               DISPLAY 'VALIDO'
               ELSE
                   DISPLAY 'INVALIDO'
               END-IF
           .

           2200-VALIDA-CNPJ.
               DISPLAY 'DIGITE O CNPJ: '
               ACCEPT CNPJ

               COMPUTE AUX = DPJ1 * 5
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ2 * 4
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ3 * 3
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ4 * 2
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ5 * 9
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ6 * 8
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ7 * 7
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ8 * 6
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ9 * 5
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ10 * 4
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ11 * 3
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ12 * 2
                       ADD AUX TO AUX2
            DISPLAY AUX2
               DIVIDE AUX2 BY 11 GIVING QUOCIENTE REMAINDER RESTO
      *         DISPLAY RESTO
               COMPUTE VALIDA-D1 = 11 - RESTO
      *         DISPLAY '1 VALIDACAO ' VALIDA-D1
               IF VALIDA-D1 LESS THAN 2
                   SET VALIDA-D1 TO 0
               END-IF



      *    SEGUNDA VALIDAÇÃO
               SET AUX2 TO 0
               COMPUTE AUX = DPJ1 * 6
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ2 * 5
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ3 * 4
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ4 * 3
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ5 * 2
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ6 * 9
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ7 * 8
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ8 * 7
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ9 * 6
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ10 * 5
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ11 * 4
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ12 * 3
                       ADD AUX TO AUX2
               COMPUTE AUX = DPJ13 * 2
                       ADD AUX TO AUX2

               DIVIDE AUX2 BY 11 GIVING QUOCIENTE REMAINDER RESTO
               COMPUTE VALIDA-D2 = RESTO - 11

               IF VALIDA-D2 LESS THAN 2
                   SET AUX3 TO 0
               ELSE
                   COMPUTE AUX3 = 11 - VALIDA-D2
               END-IF

      *    ULTIMA VALIDACAO
               IF VALIDA-D1 EQUAL TO DPJ13 AND AUX3 EQUAL TO DPJ14
                   DISPLAY 'CNPJ VALIDO.'
               ELSE
                   DISPLAY 'CNPJ INVALIDO.'
               END-IF
           .

           3000-FIM.
           STOP RUN
           .
       END PROGRAM YOUR-PROGRAM-NAME.
