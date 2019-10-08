<<<<<<< HEAD

      * Author: JULIO CESAR SILVA DE FREITAS
      * Date: 2019 09 25
      * Purpose: EXERCICIO05
      * Tectonics: cobc
      ******************************************************************
EXER02*   25-09-2019  |  CURSO        | EXECUTAR COM PERFORM.|EVERIS   *
EXER03*   25-09-2019  |COBOL EXER03   |LER ARQUIVO DE ENTRADA|CENTERS  *
EXER03*   26-09-2019  |COBOL EXER03   |GERA ARQ. DE SAIDA    |JULIO    *
EXER04*   26-09-2019  |COBOL EXER04   |ARQUIVO COM CABEÇALHO |CESAR    *
EXER05*   27-09-2019  |COBOL EXER05   |RELATORIO MENSAL      |
      *>****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCICIO06.
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
EXER02         ASSIGN TO
EXER02         'C:\cobol\COBOL\BECA\Entrada01.txt'
EXER02         ORGANISATION IS LINE SEQUENTIAL
EXER02         FILE STATUS IS WSN-FS-ENTRADA.

EXER06     SELECT ENTRADA02
EXER06         ASSIGN TO
EXER06         'C:\cobol\COBOL\BECA\Entrada02.txt'
EXER06          ORGANISATION IS LINE SEQUENTIAL
EXER06          FILE STATUS IS WSN-FS-ENTRADA02.

EXER04     SELECT SAIDA ASSIGN TO 'C:\cobol\COBOL\BECA\Saida01.txt'
EXER04            ORGANIZATION IS LINE SEQUENTIAL
EXER04            ACCESS IS SEQUENTIAL
EXER04            FILE STATUS IS WSN-FS-SAIDA.

EXER02 DATA DIVISION.
      *
EXER02 FILE SECTION.
EXER02 FD ENTRADA.
      *
       01 REG-ENTRADA.
EXER04     05 COD-CTA              PIC 9(04).
EXER04     05 TIP-CTA              PIC X(14).
EXER04     05 SLD-CTA              PIC 9(06)V99.
      *                ENTRADA02
EXER06 FD ENTRADA02.

EXER06 01 REG-ENTRADA-02.
EXER06     05 E2-COD-CTA              PIC 9(04).
EXER06     05 E2-NOM-CLI              PIC X(25).
EXER06     05 E2-COD-CID              PIC 9(04).
EXER06     05 E2-DAT-INI              PIC 9(08).
      **********************************SAÍDAS**************************
EXER03 FD SAIDA.
EXER03 01  REG-SAIDA               PIC X(84).

      ******************************************************************
      *
       WORKING-STORAGE SECTION.

EXER02 01 WCA-VARIAVEIS.
EXER02     05 WSN-FS-ENTRADA       PIC 9(02) VALUE ZEROS.
EXER03     05 WSN-FS-SAIDA         PIC 9(02) VALUE ZEROS.
EXER06     05 WSN-FS-ENTRADA02     PIC 9(02) VALUE ZEROS.
EXER06     05 WSN-FS-SAIDA02       PIC 9(02) VALUE ZEROS.

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
           05 SW-ENTRADA02         PIC X(01) VALUE 'N'.
               88 SW-FIM-NAO-02              VALUE 'N'.
               88 SW-FIM-SIM-02              VALUE 'S'.

      ***************************CABECALHO RELATORIO*******************
       01  LINE-DESIGN             PIC X(84)
               VALUES
           '*===========================================================
      -    '=======================*'.

       01  WCA-SUBTITULO            PIC X(84) VALUES
           '*               CLIENTES COM SALDO MAIOR QUE R$ 100.000,00
      -    '                       *'.
       01  WCA-RESUMO-CONTAS        PIC X(84) VALUES
           '*                     RESUMO DAS CONTAS
      -     '                       *'.

      ********************CONFIGURAÇAO DE DATA ************************
EXER05 01  WS-DATA PIC X(8).
EXER05 01  TITULO-CAB.
EXER05     05 FILLER               PIC X(70) VALUES
EXER05         '*                     RELATORIO MENSAL DO BANCO '.
EXER05     05 WS-DATA-SYSTEMA.
EXER05         10 WS-DIA           PIC X(02).
EXER05         10 FILLER           PIC X     VALUE '/'.
EXER05         10 WS-MES           PIC X(02).
EXER05         10 FILLER           PIC X     VALUE '/'.
EXER05         10 WS-ANO           PIC X(04).
               05 FILLER           PIC X(04)  VALUE '   *'.

       05 WS-DATA-CONTA.
           10 WS-ANO-CONTA           PIC X(04).
           10 WS-MES-CONTA           PIC X(02).
           10 WS-DIA-CONTA           PIC X(02).

      ******************************************************************

EXER04 01 CABECALHO.
EXER06     05 WCA-CODIGO           PIC X(06) VALUES 'CODIGO'.
EXER04     05 FILLER               PIC X(02) VALUES SPACES.
           05 WCA-NOME-CLIENTE     PIC X(15) VALUES 'NOME DO CLIENTE'.
EXER06     05 FILLER               PIC X(09) VALUES SPACES.
           05 WCA-DATA-CONTA       PIC X(10) VALUES 'DATA CONTA'.
           05 FILLER               PIC X(07) VALUES SPACES.
EXER04     05 WCA-CONTA            PIC X(14) VALUES ' TIPO DA CONTA'.
EXER04     05 FILLER               PIC X(07) VALUES SPACES.
EXER04     05 WCA-SALDO            PIC X(15) VALUES 'SALDO DA CONTA'.

EXER04 01 WSA-SAIDA.
EXER06*     05 FILLER               PIC X(3)  VALUES SPACES.
EXER04     05 S01-COD-CTA          PIC 9(04)  VALUES ZEROS.
EXER04     05 FILLER               PIC X(02)  VALUES SPACES.
EXER06     05 S01-NOME-CLI         PIC X(17)  VALUES SPACES.
           05 FILLER               PIC X(09)  VALUES SPACES.
EXER06     05 S01-DATA-CTA.
EXER06         10 WCA-DIA-CONTA    PIC 9(02) VALUE ZEROS.
EXER06         10 FILLER           PIC X     VALUE '/'.
EXER06         10 WCA-MES-CONTA    PIC 9(02) VALUES ZEROS.
EXER06         10 FILLER           PIC X     VALUE '/'.
EXER06         10 WCA-ANO-CONTA    PIC 9(04) VALUES ZEROS.
           05 FILLER               PIC X(07) VALUES SPACES.
EXER04     05 S01-TIP-CTA          PIC X(14).
EXER04     05 FILLER               PIC X(07) VALUES SPACES.
EXER04     05 FILLER               PIC X(04) VALUES 'R$  '.
EXER04     05 S01-SLD-CTA          PIC ZZZ.ZZ9,99 VALUES ZEROS.

       05 WSA-TOTAL-CC.
           10 FILLER               PIC X(15) VALUES SPACES.
EXER05     10 FILLER               PIC X(33) VALUES
EXER05         '   TOTAL DE CONTAS CORRENTES ='.
EXER05     10 WS-COUNT-CC          PIC 9(02) VALUES ZEROS.

EXER05 01 WSA-TOTAL-POUP.
           10 FILLER               PIC X(15) VALUES SPACES.
EXER05     10 FILLER               PIC X(33) VALUES
EXER05         '   TOTAL DE POUPANCAS        ='.
EXER05     10 WS-COUNT-POUP        PIC 9(02) VALUES ZEROS.


EXER05 01 WS-SALDO-ALL-CC          PIC 9(9)V99 VALUES ZEROS.
EXER05 01 WS-SALDO-ALL-CP          PIC 9(9)V99 VALUES ZEROS.
EXER05 01 WSA-SALDO-TOTAL-CC.
           10 FILLER               PIC X(31) VALUES SPACES.
EXER05     10 FILLER               PIC X(39) VALUES
EXER05         '   SALDO TOTAL DE CONTA CORRENTE = R$  '.
EXER05     10 WCA-MASCARA-MOEDA      PIC ZZZ.ZZZ.ZZ9,99 VALUES ZEROS.

EXER05 01 WSA-SALDO-TOTAL-POUP.
           10 FILLER               PIC X(31) VALUES SPACES.
EXER05     10 FILLER               PIC X(39) VALUES
EXER05         '   SALDO TOTAL DE POUPANCA       = R$'.
EXER05     10  WCA-MASC-MOEDA-POUP      PIC ZZZ.ZZZ.ZZ9,99 VALUES ZEROS.

EXER05 01 WSA-SOMA-TOTAIS          PIC 9(9)V99 VALUES ZEROS.
EXER05 01 WSA-TOTAL-CONTAS.
           10 FILLER               PIC X(31) VALUES SPACES.
EXER05     10 FILLER               PIC X(39)   VALUES
EXER05          '   SALDO TOTAL DAS CONTAS        = R$'.
EXER05     10 WCA-MASC-MOEDA-TOTAL  PIC ZZZ.ZZZ.ZZ9,99 VALUES ZEROS.

EXER05 01 WSA-DETALHE.
EXER05     10 FILLER               PIC X(84)   VALUES
EXER05     '*                  DETALHES DE GERENTE
      -    '                       *'.

EXER05 01 WSA-DATELHE              PIC X(43)   VALUES
EXER05         '  CLIENTE COM MAIOR SALDO EM CONTA CORRENTE'.
EXER05 01 WCA-DETALHE-CP           PIC X(43)   VALUES
EXER05         '  CLIENTE COM MAIOR SALDO EM CONTA POUPANCA'.
EXER05 01 WCA-DETALHE-CC-MS        PIC X(43)   VALUES
EXER05         '  CLIENTE COM MENOR SALDO EM CONTA CORRENTE'.
EXER05 01 WCA-DETALHE-CP-MS         PIC X(43)  VALUES
               '  CLIENTE COM MENOR SALDO EM POUPANCA'.
      ****** ESTRUTURA DO CLIENTE COM MAIOR SALDO EM CONTA CORRENTE*****
EXER05 01 WSA-AUXILIAR             PIC ZZZ.ZZ9,99 VALUES ZEROS.
EXER05 01 WSA-AUXILIAR-COD         PIC 9(04)      VALUES ZEROS.
       01 WSA-AUXILIAR-NOME-CLI    PIC X(17)      VALUES SPACES.
EXER05 01 WSA-MAIOR-SDO-CC.
EXER05     10 FILLER               PIC X(43)      VALUES SPACES.
EXER05     10 S02-COD-CTA          PIC 9(04)      VALUES ZEROS.
EXER05     10 FILLER               PIC X(04)      VALUES SPACES.
EXER06     10 S02-NOME-CLI         PIC X(17)      VALUES SPACES.
EXER06     10 FILLER               PIC X(6)       VALUES '   R$ '.
EXER05     10 S02-SLD-CTA          PIC ZZZ.ZZ9,99 VALUES ZEROS.
      ******************************************************************
      *        CLIENTE COM MAIOR SALDO EM CONTA POUPANÇA               *
      ******************************************************************
EXER05 01 WSA-AUXILIAR-CP          PIC ZZZ.ZZ9,99 VALUES ZEROS.
EXER05 01 WSA-AUXILIAR-COD-CP      PIC 9(4)       VALUES ZEROS.
       01 WSA-AUXILIAR-NOME-CP     PIC X(17)      VALUES ZEROS.

EXER05 01 WCA-MAIOR-SALDO-CP.
EXER05     10 FILLER               PIC X(43)      VALUES SPACES.
EXER05     10 S03-COD-CTA          PIC 9(04)      VALUES ZEROS.
           10 FILLER               PIC X(04)      VALUES SPACES.
           10 S03-NOME-CLI         PIC X(17)      VALUES SPACES.
EXER05     10 FILLER               PIC X(6)       VALUES '   R$ '.
EXER05     10 S03-SLD-CTA          PIC ZZZ.ZZ9,99 VALUES ZEROS.
      ******************************************************************
      *         CLIENTE COM MENOR SALDO EM CONTA CORRENTE              *
      ******************************************************************
EXER05 01 WSA-AUX-MENOR-CC         PIC 9(6)V99     VALUES 999999,00.
EXER05 01 WSA-AUX-MENOR-COD-CC     PIC 9(4)        VALUES ZEROS.
       01 WSA-AUX-MENOR-NOME-CC    PIC X(17)       VALUES SPACES.

EXER05 01 WCA-MENOR-SALDO-CC.
EXER05     10 FILLER               PIC X(43)       VALUES SPACES.
EXER05     10 S04-COD-CTA          PIC 9(04)       VALUES ZEROS.
           10 FILLER               PIC X(04)       VALUES SPACES.
           10 S04-NOME-CLI         PIC X(17)       VALUES SPACES.
EXER05     10 FILLER               PIC X(6)        VALUES '   R$ '.
EXER05     10 S04-SLD-CTA          PIC ZZZ.ZZ9,99  VALUES ZEROS.
      ******************************************************************
      *          CLIENTE COM MENOR SALDO EM CONTA POUPANÇA             *
      ******************************************************************
EXER05 01 WSA-AUX-MENOR-CP         PIC 9(6)V99     VALUES 999999,00.
EXER05 01 WSA-AUX-MENOR-COD-CP     PIC 9(4)        VALUES ZEROS.
       01 WSA-AUX-MENOR-NOME-CP    PIC X(17)       VALUES SPACES.

EXER05 01 WCA-MENOR-SALDO-CP.
EXER05     10 FILLER               PIC X(43)       VALUES SPACES.
EXER05     10 S05-COD-CTA          PIC 9(04)       VALUES ZEROS.
           10 FILLER               PIC X(04)       VALUES SPACES.
           10 S05-NOME-CLI         PIC X(17)       VALUES SPACES.
EXER05     10 FILLER               PIC X(6)        VALUES '   R$ '.
EXER05     10 S05-SLD-CTA          PIC ZZZ.ZZ9,99  VALUES ZEROS.
      ******************************************************************

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

EXER02     UNTIL SW-FIM-SIM

           PERFORM 2000-INSERI-LINHA-DESIGN.

EXER05     PERFORM 2100-CABECALHO-RESUMO-CONTA.

EXER05     PERFORM 2000-INSERI-LINHA-DESIGN.
EXER05     PERFORM 2200-RESUMO-TOTAL-CONTAS.
      *
           PERFORM 2300-SALDO-TOTAL-CONTAS.
           PERFORM 2400-SALDO-TOTAL-CONTAS-CP.

           PERFORM 2500-TOTAL-EM-CONTAS.

EXER05     PERFORM 2000-INSERI-LINHA-DESIGN.
EXER05     PERFORM 2600-DETALHE-GERENCIA.
EXER05     PERFORM 2700-MAIOR-SALDO-CC.
EXER05     PERFORM 2800-MAIOR-SALDO-CP.
EXER05     PERFORM 2900-MENOR-SALDO-CC.
           PERFORM 3100-MENOR-SALDO-CP.

           PERFORM 9000-FINALIZAR
              THRU 9000-FINALIZAR-FIM

           STOP RUN.
       0000-PRINCIPAL-FIM.
      *-----------------------------------------------------------------
       1000-INICIO.
      *-----------------------------------------------------------------
EXER04*          DISPLAY WCA-INICIO.
EXER02     SET SW-FIM-NAO      TO TRUE
EXER06     SET SW-FIM-NAO-02   TO TRUE.
EXER02     OPEN INPUT ENTRADA
EXER06     OPEN INPUT ENTRADA02
EXER04     OPEN OUTPUT SAIDA

EXER04*    DISPLAY 'COD ABERTURA= ' WSN-FS-ENTRADA

EXER05     ACCEPT WS-DATA FROM DATE YYYYMMDD.
EXER05     MOVE WS-DATA(1:4) TO WS-ANO.
EXER05     MOVE WS-DATA(5:2) TO WS-MES.
EXER05     MOVE WS-DATA(7:2) TO WS-DIA.

EXER05     INITIALIZE WS-COUNT-POUP.
EXER05     INITIALIZE WS-COUNT-CC.
EXER05     INITIALIZE WS-SALDO-ALL-CC.
EXER05     INITIALIZE WSA-SOMA-TOTAIS.

EXER05******************RELATORIO CABEÇALHO.***************************
EXER05     PERFORM 2000-INSERI-LINHA-DESIGN

EXER05     MOVE TITULO-CAB TO REG-SAIDA.
EXER05     WRITE REG-SAIDA.

EXER05     PERFORM 2000-INSERI-LINHA-DESIGN.
           MOVE WCA-SUBTITULO TO REG-SAIDA.
           WRITE REG-SAIDA.
           PERFORM 2000-INSERI-LINHA-DESIGN
      *****************************************************************
EXER04     MOVE CABECALHO TO REG-SAIDA.
EXER04     WRITE REG-SAIDA BEFORE 1 LINES.

EXER02     PERFORM 1100-LER-ENTRADA
EXER02        THRU 1100-LER-ENTRADA-FIM.

EXER06     PERFORM 1200-LER-ENTRADA-ARQ02
EXER06        THRU 1200-LER-ENTRADA-ARQ02-FIM.

       1000-INICIO-FIM.
           EXIT.
      ******************************************************************
      *                    E N T R A D A
      ******************************************************************
EXER02 1100-LER-ENTRADA.
EXER02     READ ENTRADA
EXER02          AT END
EXER02          SET SW-FIM-SIM TO TRUE
EXER02     END-READ.
EXER03*    DISPLAY WSN-FS-ENTRADA.
EXER02 1100-LER-ENTRADA-FIM.
            EXIT.
      ****************************LEITURA DO ARQUIVO02******************
EXER06 1200-LER-ENTRADA-ARQ02.
EXER06     READ ENTRADA02
EXER06         AT END
EXER06         SET SW-FIM-SIM-02 TO TRUE
EXER06     END-READ.
EXER06 1200-LER-ENTRADA-ARQ02-FIM.
      *     EXIT.
      *-----------------------------------------------------------------
       3000-PROCESSO.
      *-----------------------------------------------------------------

EXER06******************************************************************
EXER06*                        RESUMO DO GERENTE                       *
EXER06******************************************************************
EXER04*     DISPLAY WCA-PROCESSO
EXER04*                ' ' REG-ENTRADA
EXER06     IF COD-CTA EQUALS TO E2-COD-CTA
EXER05         IF TIP-CTA EQUALS 'Conta Corrente'
EXER05             MOVE '    C/C'  TO TIP-CTA
EXER05             ADD 1 TO WS-COUNT-CC
EXER05             COMPUTE  WS-SALDO-ALL-CC = WS-SALDO-ALL-CC + SLD-CTA
EXER05         IF SLD-CTA GREATER THAN WSA-AUXILIAR THEN
EXER05             MOVE COD-CTA    TO WSA-AUXILIAR-COD
EXER06             MOVE E2-NOM-CLI TO WSA-AUXILIAR-NOME-CLI
EXER05             MOVE SLD-CTA    TO WSA-AUXILIAR
EXER05         END-IF
EXER05         IF SLD-CTA LESS THAN WSA-AUX-MENOR-CC THEN
EXER05             MOVE COD-CTA    TO WSA-AUX-MENOR-COD-CC
EXER06             MOVE E2-NOM-CLI TO WSA-AUX-MENOR-NOME-CC
EXER05             MOVE SLD-CTA    TO WSA-AUX-MENOR-CC
EXER05         END-IF
EXER06      END-IF
EXER06       END-IF.

EXER05*     ELSE
EXER06     IF COD-CTA EQUALS TO E2-COD-CTA
EXER05         IF TIP-CTA EQUALS 'Poupanca'
EXER05             MOVE '    POUP' TO TIP-CTA
EXER05             ADD 1 TO WS-COUNT-POUP
EXER05             COMPUTE WS-SALDO-ALL-CP = WS-SALDO-ALL-CP + SLD-CTA
EXER05             IF SLD-CTA GREATER THAN WSA-AUXILIAR THEN
EXER05                 MOVE COD-CTA     TO WSA-AUXILIAR-COD-CP
EXER06                 MOVE E2-NOM-CLI  TO WSA-AUXILIAR-NOME-CP
EXER05                 MOVE SLD-CTA     TO WSA-AUXILIAR-CP
EXER05             END-IF
EXER05             IF SLD-CTA LESS THAN WSA-AUX-MENOR-CP THEN
EXER05                 MOVE COD-CTA    TO WSA-AUX-MENOR-COD-CP
EXER06                 MOVE E2-NOM-CLI TO WSA-AUX-MENOR-NOME-CP
EXER05                 MOVE SLD-CTA    TO WSA-AUX-MENOR-CP
EXER05             END-IF
EXER06         END-IF
EXER05     END-IF.
      ******************************************************************
      *                    B A T I M E N T O
      ******************************************************************
EXER06     IF COD-CTA EQUALS TO E2-COD-CTA
      *        CONDICIONAL PARA RESUMO DAS CONTAS ACIMA DE 100.000.00.
EXER05         IF SLD-CTA GREATER THAN 100000,00
EXER03             MOVE COD-CTA            TO S01-COD-CTA
EXER06             MOVE E2-NOM-CLI         TO S01-NOME-CLI
EXER06             MOVE E2-DAT-INI         TO WS-DATA-CONTA
EXER06             MOVE WS-DIA-CONTA       TO WCA-DIA-CONTA
EXER06             MOVE WS-MES-CONTA       TO WCA-MES-CONTA
EXER06             MOVE WS-ANO-CONTA       TO WCA-ANO-CONTA
EXER03             MOVE TIP-CTA            TO S01-TIP-CTA
EXER03             MOVE SLD-CTA            TO S01-SLD-CTA
EXER05             MOVE WSA-SAIDA          TO REG-SAIDA
EXER05             WRITE REG-SAIDA
EXER05             END-WRITE
EXER06         END-IF
EXER05
EXER06     END-IF.

EXER06     IF COD-CTA EQUALS TO E2-COD-CTA
                PERFORM 1100-LER-ENTRADA
EXER06            THRU 1100-LER-ENTRADA-FIM
EXER06     ELSE
EXER06         PERFORM 1200-LER-ENTRADA-ARQ02
EXER06            THRU 1200-LER-ENTRADA-ARQ02-FIM
           END-IF.

EXER04*         MOVE WSA-SAIDA TO REG-SAIDA
EXER03*         WRITE REG-SAIDA
EXER03*     END-WRITE

EXER02*     PERFORM 1100-LER-ENTRADA
EXER02*        THRU 1100-LER-ENTRADA-FIM.
      *----------------------------------------------------------------
       3000-PROCESSO-FIM.
               EXIT.
      *----------------------------------------------------------------
EXER05 2000-INSERI-LINHA-DESIGN.
EXER05     MOVE LINE-DESIGN            TO REG-SAIDA.
EXER05     WRITE REG-SAIDA.

EXER05 2100-CABECALHO-RESUMO-CONTA.
EXER05     MOVE WCA-RESUMO-CONTAS      TO REG-SAIDA.
EXER05     WRITE REG-SAIDA.

EXER05 2200-RESUMO-TOTAL-CONTAS.
EXER05     MOVE WSA-TOTAL-CC           TO REG-SAIDA.
EXER05     WRITE REG-SAIDA.
EXER05     MOVE WSA-TOTAL-POUP         TO REG-SAIDA.
EXER05     WRITE REG-SAIDA BEFORE 2 LINES.

EXER05     MOVE WS-SALDO-ALL-CC        TO WCA-MASCARA-MOEDA.
EXER05     MOVE WS-SALDO-ALL-CP        TO WCA-MASC-MOEDA-POUP.

EXER05 2300-SALDO-TOTAL-CONTAS.
EXER05     MOVE WSA-SALDO-TOTAL-CC     TO REG-SAIDA.
EXER05     WRITE REG-SAIDA.

EXER05 2400-SALDO-TOTAL-CONTAS-CP.
EXER05     MOVE WSA-SALDO-TOTAL-POUP   TO REG-SAIDA.
EXER05     WRITE REG-SAIDA BEFORE 2 LINES.

EXER05 2500-TOTAL-EM-CONTAS.
EXER05     COMPUTE  WSA-SOMA-TOTAIS =
EXER05         WS-SALDO-ALL-CC + WS-SALDO-ALL-CP.
EXER05     MOVE WSA-SOMA-TOTAIS        TO WCA-MASC-MOEDA-TOTAL.
EXER05     MOVE WSA-TOTAL-CONTAS       TO REG-SAIDA.
EXER05     WRITE REG-SAIDA BEFORE 2 LINES.

EXER05 2600-DETALHE-GERENCIA.
EXER05     MOVE WSA-DETALHE            TO REG-SAIDA
EXER05     WRITE REG-SAIDA.
EXER05     PERFORM 2000-INSERI-LINHA-DESIGN.
EXER05     MOVE WSA-DATELHE            TO REG-SAIDA.
EXER05     WRITE REG-SAIDA.
      *********************** DETALHES DO GENTE  **********************
EXER05 2700-MAIOR-SALDO-CC.
EXER05     MOVE WSA-AUXILIAR-COD       TO S02-COD-CTA.
EXER06     MOVE WSA-AUXILIAR-NOME-CLI  TO S02-NOME-CLI
EXER05     MOVE WSA-AUXILIAR           TO S02-SLD-CTA.
EXER05     MOVE WSA-MAIOR-SDO-CC       TO REG-SAIDA.
EXER05     WRITE REG-SAIDA BEFORE 2 LINES
EXER05     MOVE WCA-DETALHE-CP TO REG-SAIDA
EXER05     WRITE REG-SAIDA.

EXER05 2800-MAIOR-SALDO-CP.
EXER05     MOVE WSA-AUXILIAR-COD-CP    TO S03-COD-CTA.
EXER06     MOVE WSA-AUXILIAR-NOME-CP   TO S03-NOME-CLI
EXER05     MOVE WSA-AUXILIAR-CP        TO S03-SLD-CTA.
EXER05     MOVE WCA-MAIOR-SALDO-CP     TO REG-SAIDA.
EXER05     WRITE REG-SAIDA BEFORE 2 LINES
EXER05     MOVE WCA-DETALHE-CC-MS      TO REG-SAIDA.
EXER05     WRITE REG-SAIDA.

EXER05 2900-MENOR-SALDO-CC.
EXER05     MOVE WSA-AUX-MENOR-COD-CC   TO S04-COD-CTA.
EXER06     MOVE WSA-AUX-MENOR-NOME-CC  TO S04-NOME-CLI.
EXER05     MOVE WSA-AUX-MENOR-CC       TO S04-SLD-CTA.
EXER05     MOVE WCA-MENOR-SALDO-CC     TO REG-SAIDA
EXER05     WRITE REG-SAIDA BEFORE 2 LINES.
EXER05     MOVE WCA-DETALHE-CP-MS      TO REG-SAIDA.
EXER05     WRITE REG-SAIDA.

EXER05 3100-MENOR-SALDO-CP.
EXER05     MOVE WSA-AUX-MENOR-COD-CP   TO S05-COD-CTA.
EXER06     MOVE WSA-AUX-MENOR-NOME-CP  TO S05-NOME-CLI.
EXER05     MOVE WSA-AUX-MENOR-CP       TO S05-SLD-CTA.
EXER05     MOVE WCA-MENOR-SALDO-CP     TO REG-SAIDA.
EXER05     WRITE REG-SAIDA.

      *-----------------------------------------------------------------
       9000-FINALIZAR.
      *-----------------------------------------------------------------
EXER04*         DISPLAY WCA-FINALIZAR
EXER04     CLOSE SAIDA.
EXER02     CLOSE ENTRADA.

EXER06     CLOSE ENTRADA02.

       9000-FINALIZAR-FIM.
               EXIT.
       END PROGRAM EXERCICIO06.
=======

      * Author: JULIO CESAR SILVA DE FREITAS
      * Date: 2019 09 25
      * Purpose: EXERCICIO05
      * Tectonics: cobc
      ******************************************************************
EXER02*   25-09-2019  |  CURSO        | EXECUTAR COM PERFORM.|EVERIS   *
EXER03*   25-09-2019  |COBOL EXER03   |LER ARQUIVO DE ENTRADA|CENTERS  *
EXER03*   26-09-2019  |COBOL EXER03   |GERA ARQ. DE SAIDA    |JULIO    *
EXER04*   26-09-2019  |COBOL EXER04   |ARQUIVO COM CABEÇALHO |CESAR    *
EXER05*   27-09-2019  |COBOL EXER05   |RELATORIO MENSAL      |
      *>****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCICIO06.
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
EXER02         ASSIGN TO
EXER02         'C:\cobol\COBOL\BECA\Entrada01.txt'
EXER02         ORGANISATION IS LINE SEQUENTIAL
EXER02         FILE STATUS IS WSN-FS-ENTRADA.

EXER06     SELECT ENTRADA02
EXER06         ASSIGN TO
EXER06         'C:\cobol\COBOL\BECA\Entrada02.txt'
EXER06          ORGANISATION IS LINE SEQUENTIAL
EXER06          FILE STATUS IS WSN-FS-ENTRADA02.

EXER04     SELECT SAIDA ASSIGN TO 'C:\cobol\COBOL\BECA\Saida01.txt'
EXER04            ORGANIZATION IS LINE SEQUENTIAL
EXER04            ACCESS IS SEQUENTIAL
EXER04            FILE STATUS IS WSN-FS-SAIDA.

EXER02 DATA DIVISION.
      *
EXER02 FILE SECTION.
EXER02 FD ENTRADA.
      *
       01 REG-ENTRADA.
EXER04     05 COD-CTA              PIC 9(04).
EXER04     05 TIP-CTA              PIC X(14).
EXER04     05 SLD-CTA              PIC 9(06)V99.
      *                ENTRADA02
EXER06 FD ENTRADA02.

EXER06 01 REG-ENTRADA-02.
EXER06     05 E2-COD-CTA              PIC 9(04).
EXER06     05 E2-NOM-CLI              PIC X(25).
EXER06     05 E2-COD-CID              PIC 9(04).
EXER06     05 E2-DAT-INI              PIC 9(08).
      **********************************SAÍDAS**************************
EXER03 FD SAIDA.
EXER03 01  REG-SAIDA               PIC X(84).

      ******************************************************************
      *
       WORKING-STORAGE SECTION.

EXER02 01 WCA-VARIAVEIS.
EXER02     05 WSN-FS-ENTRADA       PIC 9(02) VALUE ZEROS.
EXER03     05 WSN-FS-SAIDA         PIC 9(02) VALUE ZEROS.
EXER06     05 WSN-FS-ENTRADA02     PIC 9(02) VALUE ZEROS.
EXER06     05 WSN-FS-SAIDA02       PIC 9(02) VALUE ZEROS.

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
           05 SW-ENTRADA02         PIC X(01) VALUE 'N'.
               88 SW-FIM-NAO-02              VALUE 'N'.
               88 SW-FIM-SIM-02              VALUE 'S'.

      ***************************CABECALHO RELATORIO*******************
       01  LINE-DESIGN             PIC X(84)
               VALUES
           '*===========================================================
      -    '=======================*'.

       01  WCA-SUBTITULO            PIC X(84) VALUES
           '*               CLIENTES COM SALDO MAIOR QUE R$ 100.000,00
      -    '                       *'.
       01  WCA-RESUMO-CONTAS        PIC X(84) VALUES
           '*                     RESUMO DAS CONTAS
      -     '                       *'.

      ********************CONFIGURAÇAO DE DATA ************************
EXER05 01  WS-DATA PIC X(8).
EXER05 01  TITULO-CAB.
EXER05     05 FILLER               PIC X(70) VALUES
EXER05         '*                     RELATORIO MENSAL DO BANCO '.
EXER05     05 WS-DATA-SYSTEMA.
EXER05         10 WS-DIA           PIC X(02).
EXER05         10 FILLER           PIC X     VALUE '/'.
EXER05         10 WS-MES           PIC X(02).
EXER05         10 FILLER           PIC X     VALUE '/'.
EXER05         10 WS-ANO           PIC X(04).
               05 FILLER           PIC X(04)  VALUE '   *'.

       05 WS-DATA-CONTA.
           10 WS-ANO-CONTA           PIC X(04).
           10 WS-MES-CONTA           PIC X(02).
           10 WS-DIA-CONTA           PIC X(02).

      ******************************************************************

EXER04 01 CABECALHO.
EXER06     05 WCA-CODIGO           PIC X(06) VALUES 'CODIGO'.
EXER04     05 FILLER               PIC X(02) VALUES SPACES.
           05 WCA-NOME-CLIENTE     PIC X(15) VALUES 'NOME DO CLIENTE'.
EXER06     05 FILLER               PIC X(09) VALUES SPACES.
           05 WCA-DATA-CONTA       PIC X(10) VALUES 'DATA CONTA'.
           05 FILLER               PIC X(07) VALUES SPACES.
EXER04     05 WCA-CONTA            PIC X(14) VALUES ' TIPO DA CONTA'.
EXER04     05 FILLER               PIC X(07) VALUES SPACES.
EXER04     05 WCA-SALDO            PIC X(15) VALUES 'SALDO DA CONTA'.

EXER04 01 WSA-SAIDA.
EXER06*     05 FILLER               PIC X(3)  VALUES SPACES.
EXER04     05 S01-COD-CTA          PIC 9(04)  VALUES ZEROS.
EXER04     05 FILLER               PIC X(02)  VALUES SPACES.
EXER06     05 S01-NOME-CLI         PIC X(17)  VALUES SPACES.
           05 FILLER               PIC X(09)  VALUES SPACES.
EXER06     05 S01-DATA-CTA.
EXER06         10 WCA-DIA-CONTA    PIC 9(02) VALUE ZEROS.
EXER06         10 FILLER           PIC X     VALUE '/'.
EXER06         10 WCA-MES-CONTA    PIC 9(02) VALUES ZEROS.
EXER06         10 FILLER           PIC X     VALUE '/'.
EXER06         10 WCA-ANO-CONTA    PIC 9(04) VALUES ZEROS.
           05 FILLER               PIC X(07) VALUES SPACES.
EXER04     05 S01-TIP-CTA          PIC X(14).
EXER04     05 FILLER               PIC X(07) VALUES SPACES.
EXER04     05 FILLER               PIC X(04) VALUES 'R$  '.
EXER04     05 S01-SLD-CTA          PIC ZZZ.ZZ9,99 VALUES ZEROS.

       05 WSA-TOTAL-CC.
           10 FILLER               PIC X(15) VALUES SPACES.
EXER05     10 FILLER               PIC X(33) VALUES
EXER05         '   TOTAL DE CONTAS CORRENTES ='.
EXER05     10 WS-COUNT-CC          PIC 9(02) VALUES ZEROS.

EXER05 01 WSA-TOTAL-POUP.
           10 FILLER               PIC X(15) VALUES SPACES.
EXER05     10 FILLER               PIC X(33) VALUES
EXER05         '   TOTAL DE POUPANCAS        ='.
EXER05     10 WS-COUNT-POUP        PIC 9(02) VALUES ZEROS.


EXER05 01 WS-SALDO-ALL-CC          PIC 9(9)V99 VALUES ZEROS.
EXER05 01 WS-SALDO-ALL-CP          PIC 9(9)V99 VALUES ZEROS.
EXER05 01 WSA-SALDO-TOTAL-CC.
           10 FILLER               PIC X(31) VALUES SPACES.
EXER05     10 FILLER               PIC X(39) VALUES
EXER05         '   SALDO TOTAL DE CONTA CORRENTE = R$  '.
EXER05     10 WCA-MASCARA-MOEDA      PIC ZZZ.ZZZ.ZZ9,99 VALUES ZEROS.

EXER05 01 WSA-SALDO-TOTAL-POUP.
           10 FILLER               PIC X(31) VALUES SPACES.
EXER05     10 FILLER               PIC X(39) VALUES
EXER05         '   SALDO TOTAL DE POUPANCA       = R$'.
EXER05     10  WCA-MASC-MOEDA-POUP      PIC ZZZ.ZZZ.ZZ9,99 VALUES ZEROS.

EXER05 01 WSA-SOMA-TOTAIS          PIC 9(9)V99 VALUES ZEROS.
EXER05 01 WSA-TOTAL-CONTAS.
           10 FILLER               PIC X(31) VALUES SPACES.
EXER05     10 FILLER               PIC X(39)   VALUES
EXER05          '   SALDO TOTAL DAS CONTAS        = R$'.
EXER05     10 WCA-MASC-MOEDA-TOTAL  PIC ZZZ.ZZZ.ZZ9,99 VALUES ZEROS.

EXER05 01 WSA-DETALHE.
EXER05     10 FILLER               PIC X(84)   VALUES
EXER05     '*                  DETALHES DE GERENTE
      -    '                       *'.

EXER05 01 WSA-DATELHE              PIC X(43)   VALUES
EXER05         '  CLIENTE COM MAIOR SALDO EM CONTA CORRENTE'.
EXER05 01 WCA-DETALHE-CP           PIC X(43)   VALUES
EXER05         '  CLIENTE COM MAIOR SALDO EM CONTA POUPANCA'.
EXER05 01 WCA-DETALHE-CC-MS        PIC X(43)   VALUES
EXER05         '  CLIENTE COM MENOR SALDO EM CONTA CORRENTE'.
EXER05 01 WCA-DETALHE-CP-MS         PIC X(43)  VALUES
               '  CLIENTE COM MENOR SALDO EM POUPANCA'.
      ****** ESTRUTURA DO CLIENTE COM MAIOR SALDO EM CONTA CORRENTE*****
EXER05 01 WSA-AUXILIAR             PIC ZZZ.ZZ9,99 VALUES ZEROS.
EXER05 01 WSA-AUXILIAR-COD         PIC 9(04)      VALUES ZEROS.
       01 WSA-AUXILIAR-NOME-CLI    PIC X(17)      VALUES SPACES.
EXER05 01 WSA-MAIOR-SDO-CC.
EXER05     10 FILLER               PIC X(43)      VALUES SPACES.
EXER05     10 S02-COD-CTA          PIC 9(04)      VALUES ZEROS.
EXER05     10 FILLER               PIC X(04)      VALUES SPACES.
EXER06     10 S02-NOME-CLI         PIC X(17)      VALUES SPACES.
EXER06     10 FILLER               PIC X(6)       VALUES '   R$ '.
EXER05     10 S02-SLD-CTA          PIC ZZZ.ZZ9,99 VALUES ZEROS.
      ******************************************************************
      *        CLIENTE COM MAIOR SALDO EM CONTA POUPANÇA               *
      ******************************************************************
EXER05 01 WSA-AUXILIAR-CP          PIC ZZZ.ZZ9,99 VALUES ZEROS.
EXER05 01 WSA-AUXILIAR-COD-CP      PIC 9(4)       VALUES ZEROS.
       01 WSA-AUXILIAR-NOME-CP     PIC X(17)      VALUES ZEROS.

EXER05 01 WCA-MAIOR-SALDO-CP.
EXER05     10 FILLER               PIC X(43)      VALUES SPACES.
EXER05     10 S03-COD-CTA          PIC 9(04)      VALUES ZEROS.
           10 FILLER               PIC X(04)      VALUES SPACES.
           10 S03-NOME-CLI         PIC X(17)      VALUES SPACES.
EXER05     10 FILLER               PIC X(6)       VALUES '   R$ '.
EXER05     10 S03-SLD-CTA          PIC ZZZ.ZZ9,99 VALUES ZEROS.
      ******************************************************************
      *         CLIENTE COM MENOR SALDO EM CONTA CORRENTE              *
      ******************************************************************
EXER05 01 WSA-AUX-MENOR-CC         PIC 9(6)V99     VALUES 999999,00.
EXER05 01 WSA-AUX-MENOR-COD-CC     PIC 9(4)        VALUES ZEROS.
       01 WSA-AUX-MENOR-NOME-CC    PIC X(17)       VALUES SPACES.

EXER05 01 WCA-MENOR-SALDO-CC.
EXER05     10 FILLER               PIC X(43)       VALUES SPACES.
EXER05     10 S04-COD-CTA          PIC 9(04)       VALUES ZEROS.
           10 FILLER               PIC X(04)       VALUES SPACES.
           10 S04-NOME-CLI         PIC X(17)       VALUES SPACES.
EXER05     10 FILLER               PIC X(6)        VALUES '   R$ '.
EXER05     10 S04-SLD-CTA          PIC ZZZ.ZZ9,99  VALUES ZEROS.
      ******************************************************************
      *          CLIENTE COM MENOR SALDO EM CONTA POUPANÇA             *
      ******************************************************************
EXER05 01 WSA-AUX-MENOR-CP         PIC 9(6)V99     VALUES 999999,00.
EXER05 01 WSA-AUX-MENOR-COD-CP     PIC 9(4)        VALUES ZEROS.
       01 WSA-AUX-MENOR-NOME-CP    PIC X(17)       VALUES SPACES.

EXER05 01 WCA-MENOR-SALDO-CP.
EXER05     10 FILLER               PIC X(43)       VALUES SPACES.
EXER05     10 S05-COD-CTA          PIC 9(04)       VALUES ZEROS.
           10 FILLER               PIC X(04)       VALUES SPACES.
           10 S05-NOME-CLI         PIC X(17)       VALUES SPACES.
EXER05     10 FILLER               PIC X(6)        VALUES '   R$ '.
EXER05     10 S05-SLD-CTA          PIC ZZZ.ZZ9,99  VALUES ZEROS.
      ******************************************************************

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

EXER02     UNTIL SW-FIM-SIM

           PERFORM 2000-INSERI-LINHA-DESIGN.

EXER05     PERFORM 2100-CABECALHO-RESUMO-CONTA.

EXER05     PERFORM 2000-INSERI-LINHA-DESIGN.
EXER05     PERFORM 2200-RESUMO-TOTAL-CONTAS.
      *
           PERFORM 2300-SALDO-TOTAL-CONTAS.
           PERFORM 2400-SALDO-TOTAL-CONTAS-CP.

           PERFORM 2500-TOTAL-EM-CONTAS.

EXER05     PERFORM 2000-INSERI-LINHA-DESIGN.
EXER05     PERFORM 2600-DETALHE-GERENCIA.
EXER05     PERFORM 2700-MAIOR-SALDO-CC.
EXER05     PERFORM 2800-MAIOR-SALDO-CP.
EXER05     PERFORM 2900-MENOR-SALDO-CC.
           PERFORM 3100-MENOR-SALDO-CP.

           PERFORM 9000-FINALIZAR
              THRU 9000-FINALIZAR-FIM

           STOP RUN.
       0000-PRINCIPAL-FIM.
      *-----------------------------------------------------------------
       1000-INICIO.
      *-----------------------------------------------------------------
EXER04*          DISPLAY WCA-INICIO.
EXER02     SET SW-FIM-NAO      TO TRUE
EXER06     SET SW-FIM-NAO-02   TO TRUE.
EXER02     OPEN INPUT ENTRADA
EXER06     OPEN INPUT ENTRADA02
EXER04     OPEN OUTPUT SAIDA

EXER04*    DISPLAY 'COD ABERTURA= ' WSN-FS-ENTRADA

EXER05     ACCEPT WS-DATA FROM DATE YYYYMMDD.
EXER05     MOVE WS-DATA(1:4) TO WS-ANO.
EXER05     MOVE WS-DATA(5:2) TO WS-MES.
EXER05     MOVE WS-DATA(7:2) TO WS-DIA.

EXER05     INITIALIZE WS-COUNT-POUP.
EXER05     INITIALIZE WS-COUNT-CC.
EXER05     INITIALIZE WS-SALDO-ALL-CC.
EXER05     INITIALIZE WSA-SOMA-TOTAIS.

EXER05******************RELATORIO CABEÇALHO.***************************
EXER05     PERFORM 2000-INSERI-LINHA-DESIGN

EXER05     MOVE TITULO-CAB TO REG-SAIDA.
EXER05     WRITE REG-SAIDA.

EXER05     PERFORM 2000-INSERI-LINHA-DESIGN.
           MOVE WCA-SUBTITULO TO REG-SAIDA.
           WRITE REG-SAIDA.
           PERFORM 2000-INSERI-LINHA-DESIGN
      *****************************************************************
EXER04     MOVE CABECALHO TO REG-SAIDA.
EXER04     WRITE REG-SAIDA BEFORE 1 LINES.

EXER02     PERFORM 1100-LER-ENTRADA
EXER02        THRU 1100-LER-ENTRADA-FIM.

EXER06     PERFORM 1200-LER-ENTRADA-ARQ02
EXER06        THRU 1200-LER-ENTRADA-ARQ02-FIM.

       1000-INICIO-FIM.
           EXIT.
      ******************************************************************
      *                    E N T R A D A
      ******************************************************************
EXER02 1100-LER-ENTRADA.
EXER02     READ ENTRADA
EXER02          AT END
EXER02          SET SW-FIM-SIM TO TRUE
EXER02     END-READ.
EXER03*    DISPLAY WSN-FS-ENTRADA.
EXER02 1100-LER-ENTRADA-FIM.
            EXIT.
      ****************************LEITURA DO ARQUIVO02******************
EXER06 1200-LER-ENTRADA-ARQ02.
EXER06     READ ENTRADA02
EXER06         AT END
EXER06         SET SW-FIM-SIM-02 TO TRUE
EXER06     END-READ.
EXER06 1200-LER-ENTRADA-ARQ02-FIM.
      *     EXIT.
      *-----------------------------------------------------------------
       3000-PROCESSO.
      *-----------------------------------------------------------------

EXER06******************************************************************
EXER06*                        RESUMO DO GERENTE                       *
EXER06******************************************************************
EXER04*     DISPLAY WCA-PROCESSO
EXER04*                ' ' REG-ENTRADA
EXER06     IF COD-CTA EQUALS TO E2-COD-CTA
EXER05         IF TIP-CTA EQUALS 'Conta Corrente'
EXER05             MOVE '    C/C'  TO TIP-CTA
EXER05             ADD 1 TO WS-COUNT-CC
EXER05             COMPUTE  WS-SALDO-ALL-CC = WS-SALDO-ALL-CC + SLD-CTA
EXER05         IF SLD-CTA GREATER THAN WSA-AUXILIAR THEN
EXER05             MOVE COD-CTA    TO WSA-AUXILIAR-COD
EXER06             MOVE E2-NOM-CLI TO WSA-AUXILIAR-NOME-CLI
EXER05             MOVE SLD-CTA    TO WSA-AUXILIAR
EXER05         END-IF
EXER05         IF SLD-CTA LESS THAN WSA-AUX-MENOR-CC THEN
EXER05             MOVE COD-CTA    TO WSA-AUX-MENOR-COD-CC
EXER06             MOVE E2-NOM-CLI TO WSA-AUX-MENOR-NOME-CC
EXER05             MOVE SLD-CTA    TO WSA-AUX-MENOR-CC
EXER05         END-IF
EXER06      END-IF
EXER06       END-IF.

EXER05*     ELSE
EXER06     IF COD-CTA EQUALS TO E2-COD-CTA
EXER05         IF TIP-CTA EQUALS 'Poupanca'
EXER05             MOVE '    POUP' TO TIP-CTA
EXER05             ADD 1 TO WS-COUNT-POUP
EXER05             COMPUTE WS-SALDO-ALL-CP = WS-SALDO-ALL-CP + SLD-CTA
EXER05             IF SLD-CTA GREATER THAN WSA-AUXILIAR THEN
EXER05                 MOVE COD-CTA     TO WSA-AUXILIAR-COD-CP
EXER06                 MOVE E2-NOM-CLI  TO WSA-AUXILIAR-NOME-CP
EXER05                 MOVE SLD-CTA     TO WSA-AUXILIAR-CP
EXER05             END-IF
EXER05             IF SLD-CTA LESS THAN WSA-AUX-MENOR-CP THEN
EXER05                 MOVE COD-CTA    TO WSA-AUX-MENOR-COD-CP
EXER06                 MOVE E2-NOM-CLI TO WSA-AUX-MENOR-NOME-CP
EXER05                 MOVE SLD-CTA    TO WSA-AUX-MENOR-CP
EXER05             END-IF
EXER06         END-IF
EXER05     END-IF.
      ******************************************************************
      *                    B A T I M E N T O
      ******************************************************************
EXER06     IF COD-CTA EQUALS TO E2-COD-CTA
      *        CONDICIONAL PARA RESUMO DAS CONTAS ACIMA DE 100.000.00.
EXER05         IF SLD-CTA GREATER THAN 100000,00
EXER03             MOVE COD-CTA            TO S01-COD-CTA
EXER06             MOVE E2-NOM-CLI         TO S01-NOME-CLI
EXER06             MOVE E2-DAT-INI         TO WS-DATA-CONTA
EXER06             MOVE WS-DIA-CONTA       TO WCA-DIA-CONTA
EXER06             MOVE WS-MES-CONTA       TO WCA-MES-CONTA
EXER06             MOVE WS-ANO-CONTA       TO WCA-ANO-CONTA
EXER03             MOVE TIP-CTA            TO S01-TIP-CTA
EXER03             MOVE SLD-CTA            TO S01-SLD-CTA
EXER05             MOVE WSA-SAIDA          TO REG-SAIDA
EXER05             WRITE REG-SAIDA
EXER05             END-WRITE
EXER06         END-IF
EXER05
EXER06     END-IF.

EXER06     IF COD-CTA EQUALS TO E2-COD-CTA
                PERFORM 1100-LER-ENTRADA
EXER06            THRU 1100-LER-ENTRADA-FIM
EXER06     ELSE
EXER06         PERFORM 1200-LER-ENTRADA-ARQ02
EXER06            THRU 1200-LER-ENTRADA-ARQ02-FIM
           END-IF.

EXER04*         MOVE WSA-SAIDA TO REG-SAIDA
EXER03*         WRITE REG-SAIDA
EXER03*     END-WRITE

EXER02*     PERFORM 1100-LER-ENTRADA
EXER02*        THRU 1100-LER-ENTRADA-FIM.
      *----------------------------------------------------------------
       3000-PROCESSO-FIM.
               EXIT.
      *----------------------------------------------------------------
EXER05 2000-INSERI-LINHA-DESIGN.
EXER05     MOVE LINE-DESIGN            TO REG-SAIDA.
EXER05     WRITE REG-SAIDA.

EXER05 2100-CABECALHO-RESUMO-CONTA.
EXER05     MOVE WCA-RESUMO-CONTAS      TO REG-SAIDA.
EXER05     WRITE REG-SAIDA.

EXER05 2200-RESUMO-TOTAL-CONTAS.
EXER05     MOVE WSA-TOTAL-CC           TO REG-SAIDA.
EXER05     WRITE REG-SAIDA.
EXER05     MOVE WSA-TOTAL-POUP         TO REG-SAIDA.
EXER05     WRITE REG-SAIDA BEFORE 2 LINES.

EXER05     MOVE WS-SALDO-ALL-CC        TO WCA-MASCARA-MOEDA.
EXER05     MOVE WS-SALDO-ALL-CP        TO WCA-MASC-MOEDA-POUP.

EXER05 2300-SALDO-TOTAL-CONTAS.
EXER05     MOVE WSA-SALDO-TOTAL-CC     TO REG-SAIDA.
EXER05     WRITE REG-SAIDA.

EXER05 2400-SALDO-TOTAL-CONTAS-CP.
EXER05     MOVE WSA-SALDO-TOTAL-POUP   TO REG-SAIDA.
EXER05     WRITE REG-SAIDA BEFORE 2 LINES.

EXER05 2500-TOTAL-EM-CONTAS.
EXER05     COMPUTE  WSA-SOMA-TOTAIS =
EXER05         WS-SALDO-ALL-CC + WS-SALDO-ALL-CP.
EXER05     MOVE WSA-SOMA-TOTAIS        TO WCA-MASC-MOEDA-TOTAL.
EXER05     MOVE WSA-TOTAL-CONTAS       TO REG-SAIDA.
EXER05     WRITE REG-SAIDA BEFORE 2 LINES.

EXER05 2600-DETALHE-GERENCIA.
EXER05     MOVE WSA-DETALHE            TO REG-SAIDA
EXER05     WRITE REG-SAIDA.
EXER05     PERFORM 2000-INSERI-LINHA-DESIGN.
EXER05     MOVE WSA-DATELHE            TO REG-SAIDA.
EXER05     WRITE REG-SAIDA.
      *********************** DETALHES DO GENTE  **********************
EXER05 2700-MAIOR-SALDO-CC.
EXER05     MOVE WSA-AUXILIAR-COD       TO S02-COD-CTA.
EXER06     MOVE WSA-AUXILIAR-NOME-CLI  TO S02-NOME-CLI
EXER05     MOVE WSA-AUXILIAR           TO S02-SLD-CTA.
EXER05     MOVE WSA-MAIOR-SDO-CC       TO REG-SAIDA.
EXER05     WRITE REG-SAIDA BEFORE 2 LINES
EXER05     MOVE WCA-DETALHE-CP TO REG-SAIDA
EXER05     WRITE REG-SAIDA.

EXER05 2800-MAIOR-SALDO-CP.
EXER05     MOVE WSA-AUXILIAR-COD-CP    TO S03-COD-CTA.
EXER06     MOVE WSA-AUXILIAR-NOME-CP   TO S03-NOME-CLI
EXER05     MOVE WSA-AUXILIAR-CP        TO S03-SLD-CTA.
EXER05     MOVE WCA-MAIOR-SALDO-CP     TO REG-SAIDA.
EXER05     WRITE REG-SAIDA BEFORE 2 LINES
EXER05     MOVE WCA-DETALHE-CC-MS      TO REG-SAIDA.
EXER05     WRITE REG-SAIDA.

EXER05 2900-MENOR-SALDO-CC.
EXER05     MOVE WSA-AUX-MENOR-COD-CC   TO S04-COD-CTA.
EXER06     MOVE WSA-AUX-MENOR-NOME-CC  TO S04-NOME-CLI.
EXER05     MOVE WSA-AUX-MENOR-CC       TO S04-SLD-CTA.
EXER05     MOVE WCA-MENOR-SALDO-CC     TO REG-SAIDA
EXER05     WRITE REG-SAIDA BEFORE 2 LINES.
EXER05     MOVE WCA-DETALHE-CP-MS      TO REG-SAIDA.
EXER05     WRITE REG-SAIDA.

EXER05 3100-MENOR-SALDO-CP.
EXER05     MOVE WSA-AUX-MENOR-COD-CP   TO S05-COD-CTA.
EXER06     MOVE WSA-AUX-MENOR-NOME-CP  TO S05-NOME-CLI.
EXER05     MOVE WSA-AUX-MENOR-CP       TO S05-SLD-CTA.
EXER05     MOVE WCA-MENOR-SALDO-CP     TO REG-SAIDA.
EXER05     WRITE REG-SAIDA.

      *-----------------------------------------------------------------
       9000-FINALIZAR.
      *-----------------------------------------------------------------
EXER04*         DISPLAY WCA-FINALIZAR
EXER04     CLOSE SAIDA.
EXER02     CLOSE ENTRADA.

EXER06     CLOSE ENTRADA02.

       9000-FINALIZAR-FIM.
               EXIT.
       END PROGRAM EXERCICIO06.
>>>>>>> fc4eb318f1bf419679fc59f0577be2685a746e7a
