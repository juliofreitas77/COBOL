      ******************************************************************
      * Author: JULIO CESAR SILVA DE FREITAS
      * Date: 2019 09 25
      * Purpose: EXERCICIO04
      * Tectonics: cobc
      ******************************************************************
EXER02*> 1  25-09-2019 ][  CURSO        * EXECUTAR COM PERFORM.
EXER02*> 1             ][  COBOL        *
EXER03*> 1 EXERCICIO03 ][  EXERCICIO03  *
EXER04*> 1 EXERCICIO04 ][               *
      *>****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCICIO04.
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
EXER02            ASSIGN TO
EXER02            'C:\cobol\BECA\Entrada01.txt'
EXER02            ORGANISATION IS LINE SEQUENTIAL
EXER02            FILE STATUS IS WSN-FS-ENTRADA.

EXER04     SELECT SAIDA ASSIGN TO 'C:\cobol\BECA\Saida01.txt'
EXER04            ORGANIZATION IS LINE SEQUENTIAL
EXER04            ACCESS IS SEQUENTIAL
EXER04            FILE STATUS IS WSN-FS-SAIDA.

EXER02 DATA DIVISION.
      *
EXER02 FILE SECTION.
EXER02 FD ENTRADA.
EXER02* 01 REG-ENTRADA        PIC X(28).
       01 REG-ENTRADA.
EXER04     05 COD-CTA              PIC 9(04).
EXER04     05 TIP-CTA              PIC X(14).
EXER04     05 SLD-CTA              PIC 9(06)V99.
      *
EXER03 FD SAIDA.
EXER03 01  REG-SAIDA               PIC X(39).
      *
       WORKING-STORAGE SECTION.

EXER02 01 WCA-VARIAVEIS.
EXER02     05 WSN-FS-ENTRADA       PIC 9(02) VALUE ZEROS.
EXER03     05 WSN-FS-SAIDA         PIC 9(02) VALUE ZEROS.

       01 WCA-CONSTANTES.
           05 WCA-PRINCIPAL        PIC X(10) VALUE 'PRINCIPAL'.
           05 WCA-INICIO           PIC X(10) VALUE 'INICIO'.
           05 WCA-PROCESSO         PIC X(10) VALUE 'PROCESSO'.
           05 WCA-FINALIZAR        PIC X(10) VALUE 'FINALIZAR'.
EXER02     05 WCA-LER-ENTRADA      PIC X(12) VALUE 'LER-ENTRADA'.

EXER02 01 WSW-SWITCHES.
EXER02     05 SW-ENTRADA           PIC X(01) VALUE 'N'.
EXER02         88 SW-FIM-NAO                 VALUE 'N'.
EXER02         88 SW-FIM-SIM                 VALUE 'S'.

EXER04 01 CABECALHO.
EXER04     05 WCA-CODIGO           PIC X(06) VALUES 'CODIGO'.
EXER04     05 FILLER               PIC X     VALUES ' '.
EXER04     05 WCA-CONTA            PIC X(14) VALUES ' TIPO DA CONTA'.
EXER04     05 FILLER               PIC X(03) VALUES SPACES.
EXER04     05 WCA-SALDO            PIC X(15) VALUES 'SALDO DA CONTA'.

EXER04 01 WSA-SAIDA.
EXER04     05 S01-COD-CTA          PIC 9(07) VALUES ZEROS.
EXER04     05 FILLER               PIC X(1)  VALUES SPACES.
EXER04     05 S01-TIP-CTA          PIC X(14).
EXER04     05 FILLER               PIC X(1)  VALUES SPACES.
EXER04     05 FILLER               PIC X(3)  VALUE ' R$'.
EXER04     05 S01-SLD-CTA          PIC ZZZZ.ZZ9,99 VALUES ZEROS.

      *----------------------------
       PROCEDURE DIVISION.
      *----------------------------
       0000-PRINCIPAL.
      *
EXER04*    DISPLAY WCA-PRINCIPAL.

           PERFORM 1000-INICIO
              THRU 1000-INICIO-FIM
      *
           PERFORM 3000-PROCESSO
              THRU 3000-PROCESSO-FIM
EXER02       UNTIL SW-FIM-SIM
      *
           PERFORM 9000-FINALIZAR
              THRU 9000-FINALIZAR-FIM

           STOP RUN.
       0000-PRINCIPAL-FIM.
      *-----------------------------------------------------------------
       1000-INICIO.
      *-----------------------------------------------------------------
EXER04*          DISPLAY WCA-INICIO.
EXER02     SET SW-FIM-NAO TO TRUE
EXER02     OPEN INPUT ENTRADA

EXER04*    DISPLAY 'COD ABERTURA= ' WSN-FS-ENTRADA

EXER04     OPEN OUTPUT SAIDA

EXER04     MOVE CABECALHO TO REG-SAIDA.
EXER04     WRITE REG-SAIDA BEFORE 2 LINES.

EXER02     PERFORM 1100-LER-ENTRADA
EXER02        THRU 1100-LER-ENTRADA-FIM.

       1000-INICIO-FIM.
           EXIT.

EXER02 1100-LER-ENTRADA.
EXER02     READ ENTRADA
EXER02          AT END
EXER02          SET SW-FIM-SIM TO TRUE
EXER02          END-READ.
EXER03*>   DISPLAY WSN-FS-ENTRADA.

EXER02 1100-LER-ENTRADA-FIM.
               EXIT.
      *-----------------------------------------------------------------
           3000-PROCESSO.
      *-----------------------------------------------------------------
EXER04*     DISPLAY WCA-PROCESSO
EXER04*                ' ' REG-ENTRADA

EXER03     MOVE COD-CTA TO S01-COD-CTA
EXER03     MOVE TIP-CTA TO S01-TIP-CTA
EXER03     MOVE SLD-CTA TO S01-SLD-CTA

EXER04     MOVE WSA-SAIDA TO REG-SAIDA

EXER03     WRITE REG-SAIDA
EXER03     END-WRITE

EXER02     PERFORM 1100-LER-ENTRADA
EXER02        THRU 1100-LER-ENTRADA-FIM.

           3000-PROCESSO-FIM.
               EXIT.
      *-----------------------------------------------------------------
           9000-FINALIZAR.
      *-----------------------------------------------------------------
EXER04*         DISPLAY WCA-FINALIZAR
EXER04     CLOSE SAIDA.
EXER02     CLOSE ENTRADA.
           9000-FINALIZAR-FIM.
               EXIT.
       END PROGRAM EXERCICIO04.
