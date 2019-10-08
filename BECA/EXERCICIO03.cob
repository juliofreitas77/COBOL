      ******************************************************************
      * Author: JULIO CESAR SILVA DE FREITAS
      * Date: 2019 09 25
      * Purpose: EXERCICIO03
      * Tectonics: cobc
      ******************************************************************
EXER02*> 1  25-09-2019 ][  CURSO        * EXECUTAR COM PERFORM.
EXER02*> 1             ][  COBOL        *
EXER02*> 1 EXERCICIO03 ][  EXERCICIO03  *
EXER02*> 1             ][               *
      *>****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCICIO03.
       ENVIRONMENT DIVISION.
      ******************************************************************
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      ******************************************************************
       INPUT-OUTPUT SECTION.
      ******************************************************************
       FILE-CONTROL.
      *
EXER02     SELECT ENTRADA
EXER02           ASSIGN TO
EXER02                'C:\cobol\BECA\Entrada01.txt'
                       ORGANISATION IS LINE SEQUENTIAL
EXER02                 FILE STATUS IS WSN-FS-ENTRADA.

           SELECT STUDENTFILE ASSIGN TO 'C:\cobol\BECA\Saida01.txt'
                 ORGANIZATION IS LINE SEQUENTIAL
                 ACCESS IS SEQUENTIAL
                 FILE STATUS IS WSN-FS-SAIDA.

EXER02 DATA DIVISION.
EXER02 FILE SECTION.
EXER02 FD  ENTRADA.
EXER02 01  REG-ENTRADA             PIC X(28).

EXER03 FD STUDENTFILE.
EXER03 01  REG-SAIDA               PIC X(28).
      *
       WORKING-STORAGE SECTION.

EXER02 01  WCA-VARIAVEIS.
EXER02     05 WSN-FS-ENTRADA       PIC 9(02) VALUE ZEROS.
           05 WSN-FS-SAIDA         PIC 9(02) VALUE ZEROS.

       01  WCA-CONSTANTES.
           05  WCA-PRINCIPAL       PIC X(10) VALUE 'PRINCIPAL'.
           05  WCA-INICIO          PIC X(10) VALUE 'INICIO'.
           05  WCA-PROCESSO        PIC X(10) VALUE 'PROCESSO'.
           05  WCA-FINALIZAR       PIC X(10) VALUE 'FINALIZAR'.
EXER02     05  WCA-LER-ENTRADA     PIC X(12) VALUE 'LER-ENTRADA'.

EXER02 01  WSW-SWITCHES.
EXER02     05 SW-ENTRADA           PIC X(01) VALUE 'N'.
EXER02         88 SW-FIM-NAO                 VALUE 'N'.
EXER02         88 SW-FIM-SIM                 VALUE 'S'.

      *------------------------
       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY WCA-PRINCIPAL.

                PERFORM 1000-INICIO
                   THRU 1000-INICIO-FIM
      *
                PERFORM 3000-PROCESSO
                   THRU 3000-PROCESSO-FIM
EXER02            UNTIL SW-FIM-SIM
      *
                PERFORM 9000-FINALIZAR
                   THRU 9000-FINALIZAR-FIM

           STOP RUN.
       0000-PRINCIPAL-FIM.

      *----------------------------------------------------------------
       1000-INICIO.
      *----------------------------------------------------------------
           DISPLAY WCA-INICIO.
           SET SW-FIM-NAO TO TRUE
           OPEN INPUT ENTRADA

EXER03     DISPLAY 'COD ABERTURA= ' WSN-FS-ENTRADA

EXER03     OPEN OUTPUT STUDENTFILE

           PERFORM 1100-LER-ENTRADA
              THRU 1100-LER-ENTRADA-FIM.

       1000-INICIO-FIM.
           EXIT.

       1100-LER-ENTRADA.
EXER02         READ ENTRADA
EXER02             AT END
EXER02                SET SW-FIM-SIM TO TRUE
EXER02          END-READ.
EXER03*           DISPLAY WSN-FS-ENTRADA.

EXER02 1100-LER-ENTRADA-FIM.
EXER02         EXIT.
      *-----------------------------------------------------------------
           3000-PROCESSO.
      *-----------------------------------------------------------------
EXER02     DISPLAY WCA-PROCESSO
EXER02                 ' ' REG-ENTRADA

EXER03     MOVE REG-ENTRADA TO REG-SAIDA
EXER03     WRITE REG-SAIDA
EXER03     END-WRITE

EXER02     PERFORM 1100-LER-ENTRADA
EXER02        THRU 1100-LER-ENTRADA-FIM.

           3000-PROCESSO-FIM.
               EXIT.
      *-----------------------------------------------------------------
           9000-FINALIZAR.
      *-----------------------------------------------------------------
           DISPLAY WCA-FINALIZAR
EXER03     CLOSE STUDENTFILE
EXER02     CLOSE ENTRADA.
           9000-FINALIZAR-FIM.
               EXIT.
       END PROGRAM EXERCICIO03.
