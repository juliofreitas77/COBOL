<<<<<<< HEAD
      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. EXERCICIO02.
      ******************************************************************
      * 25/09/2019 | CURSO     | INCLUIR A LEITURA DO      | EVERIS
      *  EXERC 02  | EXERC 02  |   ARQUIVO DE ENTRADA01    | JULIO CESAR
      *            |           |                           |
      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      ******************************************************************
       INPUT-OUTPUT SECTION.
      ******************************************************************
EXER02     FILE-CONTROL.
EXER02     SELECT ENTRADA
EXER02         ASSIGN TO
EXER02             'C:\cobol\BECA\Entrada01.txt'
EXER02                 ORGANIZATION IS LINE SEQUENTIAL
EXER02                  FILE STATUS  IS WSN-FS-ENTRADA.

       DATA DIVISION.

       FILE SECTION.

EXER02 FD ENTRADA.

EXER02 01 REG-ENTRADA                  PIC X(28).

       WORKING-STORAGE SECTION.
EXER02 01 WCA-VARIAVEIS.
EXER02     05 WSN-FS-ENTRADA           PIC 9(02) VALUE ZEROS.

       01 WCA-CONSTANTES.
           05 WCA-PRINCIPAL            PIC X(10) VALUE 'PRINCIPAL'.
           05 WCA-INICIO               PIC X(10) VALUE 'INICIO'.
           05 WCA-FINALIZAR            PIC X(10) VALUE 'FINALIZAR'.
           05 WCA-PROCESSO             PIC X(10) VALUE 'PROCESSO'.
EXER02     05 WCA-LER-ENTRADA          PIC X(12) VALUE 'LER-ENTRADA'.

EXER02 01 WSW-SWITCHES.
EXER02     05 SW-ENTRADA               PIC X(01) VALUE 'N'.
EXER02         88 SW-FIM-NAO                     VALUE 'N'.
EXER02         88 SW-FIM-SIM                     VALUE 'S'.
      *__________________________
       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY WCA-PRINCIPAL

           PERFORM 1000-INICIO
              THRU 1000-INICIO-FIM

           PERFORM 2000-PROCESSO
              THRU 2000-PROCESSO-FIM
             UNTIL SW-FIM-SIM

            PERFORM 3000-FINALIZAR
               THRU 3000-FINALIZAR-FIM

            STOP RUN.
      ******************************************************************
       1000-INICIO.
      ******************************************************************
           DISPLAY WCA-INICIO
EXER02     SET SW-FIM-NAO TO TRUE
EXER02     OPEN INPUT ENTRADA

EXER02     PERFORM 1100-LER-ENTRADA
EXER02        THRU 1100-LER-ENTRADA-FIM
              .
       1000-INICIO-FIM.
EXER02      EXIT.

EXER02 1100-LER-ENTRADA.

EXER02     READ ENTRADA
EXER02     AT END
EXER02        SET SW-FIM-SIM TO TRUE
EXER02     END-READ
EXER02     .
EXER02 1100-LER-ENTRADA-FIM.
EXER02     EXIT.
      *_________________________________________________________________
       2000-PROCESSO.
      *-----------------------------------------------------------------
EXER02     DISPLAY WCA-PROCESSO
EXER02             REG-ENTRADA

EXER02     PERFORM 1100-LER-ENTRADA
EXER02        THRU 1100-LER-ENTRADA-FIM
           .
       2000-PROCESSO-FIM.
           EXIT.
      *_________________________________________________________________
       3000-FINALIZAR.
      *-----------------------------------------------------------------
           DISPLAY WCA-FINALIZAR
EXER02     CLOSE ENTRADA.
       3000-FINALIZAR-FIM.
            EXIT.
       END PROGRAM EXERCICIO02.
=======
      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. EXERCICIO02.
      ******************************************************************
      * 25/09/2019 | CURSO     | INCLUIR A LEITURA DO      | EVERIS
      *  EXERC 02  | EXERC 02  |   ARQUIVO DE ENTRADA01    | JULIO CESAR
      *            |           |                           |
      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      ******************************************************************
       INPUT-OUTPUT SECTION.
      ******************************************************************
EXER02     FILE-CONTROL.
EXER02     SELECT ENTRADA
EXER02         ASSIGN TO
EXER02             'C:\cobol\BECA\Entrada01.txt'
EXER02                 ORGANIZATION IS LINE SEQUENTIAL
EXER02                  FILE STATUS  IS WSN-FS-ENTRADA.

       DATA DIVISION.

       FILE SECTION.

EXER02 FD ENTRADA.

EXER02 01 REG-ENTRADA                  PIC X(28).

       WORKING-STORAGE SECTION.
EXER02 01 WCA-VARIAVEIS.
EXER02     05 WSN-FS-ENTRADA           PIC 9(02) VALUE ZEROS.

       01 WCA-CONSTANTES.
           05 WCA-PRINCIPAL            PIC X(10) VALUE 'PRINCIPAL'.
           05 WCA-INICIO               PIC X(10) VALUE 'INICIO'.
           05 WCA-FINALIZAR            PIC X(10) VALUE 'FINALIZAR'.
           05 WCA-PROCESSO             PIC X(10) VALUE 'PROCESSO'.
EXER02     05 WCA-LER-ENTRADA          PIC X(12) VALUE 'LER-ENTRADA'.

EXER02 01 WSW-SWITCHES.
EXER02     05 SW-ENTRADA               PIC X(01) VALUE 'N'.
EXER02         88 SW-FIM-NAO                     VALUE 'N'.
EXER02         88 SW-FIM-SIM                     VALUE 'S'.
      *__________________________
       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY WCA-PRINCIPAL

           PERFORM 1000-INICIO
              THRU 1000-INICIO-FIM

           PERFORM 2000-PROCESSO
              THRU 2000-PROCESSO-FIM
             UNTIL SW-FIM-SIM

            PERFORM 3000-FINALIZAR
               THRU 3000-FINALIZAR-FIM

            STOP RUN.
      ******************************************************************
       1000-INICIO.
      ******************************************************************
           DISPLAY WCA-INICIO
EXER02     SET SW-FIM-NAO TO TRUE
EXER02     OPEN INPUT ENTRADA

EXER02     PERFORM 1100-LER-ENTRADA
EXER02        THRU 1100-LER-ENTRADA-FIM
              .
       1000-INICIO-FIM.
EXER02      EXIT.

EXER02 1100-LER-ENTRADA.

EXER02     READ ENTRADA
EXER02     AT END
EXER02        SET SW-FIM-SIM TO TRUE
EXER02     END-READ
EXER02     .
EXER02 1100-LER-ENTRADA-FIM.
EXER02     EXIT.
      *_________________________________________________________________
       2000-PROCESSO.
      *-----------------------------------------------------------------
EXER02     DISPLAY WCA-PROCESSO
EXER02             REG-ENTRADA

EXER02     PERFORM 1100-LER-ENTRADA
EXER02        THRU 1100-LER-ENTRADA-FIM
           .
       2000-PROCESSO-FIM.
           EXIT.
      *_________________________________________________________________
       3000-FINALIZAR.
      *-----------------------------------------------------------------
           DISPLAY WCA-FINALIZAR
EXER02     CLOSE ENTRADA.
       3000-FINALIZAR-FIM.
            EXIT.
       END PROGRAM EXERCICIO02.
>>>>>>> fc4eb318f1bf419679fc59f0577be2685a746e7a
