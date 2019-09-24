       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGCOB02.
      ********************************************************
      *AREA DE COMENTARIOS - REMARKS
      *AUTHOR: Julio
      *OBJETIVO: RECEBER E IMPRIMIR STRING
      *DATA:
      ********************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WRK-NOME PIC X(20) VALUE SPACES.

       PROCEDURE DIVISION.
      *****AREA DE COMANDOS 12 - 72
           ACCEPT WRK-NOME FROM CONSOLE.
           DISPLAY 'Nome...' WRK-NOME.
           STOP RUN.
