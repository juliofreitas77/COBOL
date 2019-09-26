       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGCOBVARYING.
      ******************************************************
      ***   AREA DE COMENTARIOS - REMARKS
      ***   OBJETIVO DO PROGRAMA = PERFORM COM REPETICOES FINITAS
      ***                            USANDO VARYING
      ***   AUTOR: JULIO CESAR
      ***   DATA : 26/09/2019
      ******************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WRK-NUMERO      PIC 9(02)     VALUE ZEROS.
       77 WRK-CONTADOR    PIC 9(02)     VALUE ZEROS.
       77 WRK-RESUL       PIC 9(03)     VALUE ZEROS.
       PROCEDURE DIVISION.
       0001-PRINCIPAL.
            PERFORM 0100-INICIALIZAR.
             IF WRK-NUMERO GREATER 0
               PERFORM 0200-PROCESSAR
               PERFORM 0300-FINALIZAR
            END-IF.
            STOP RUN.
       0100-INICIALIZAR.
            DISPLAY 'DIGITE O NUMERO..... : '.
            ACCEPT WRK-NUMERO.
       0200-PROCESSAR.
           PERFORM VARYING WRK-CONTADOR
               FROM 1 BY 1 UNTIL WRK-CONTADOR > 10
                 COMPUTE WRK-RESUL = WRK-NUMERO * WRK-CONTADOR
                 DISPLAY WRK-NUMERO ' X ' WRK-CONTADOR ' = ' WRK-RESUL
            END-PERFORM.
       0300-FINALIZAR.
             DISPLAY  '--------- SAIDA DE DADOS ------------'.
             DISPLAY ' FIM DE PROCESSAMENTO '.

       END PROGRAM PROGCOBVARYING.
