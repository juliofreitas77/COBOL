       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGCOB03.
      ********************************************************
      *AREA DE COMENTARIOS - REMARKS
      *AUTHOR: Julio
      *OBJETIVO: RECEBER E IMPRIMIR VARIAVEL DO SISTEMA
      *UTILIZAR VARIAVEIS NIVEL 01.02... (ESTRUTURADA)
      ********************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WRK-DATA.
          02 WRK-ANO PIC 9(04) VALUES ZEROS.
          02 WRK-MES PIC 9(02) VALUES ZEROS.
          02 WRK-DIA PIC 9(02) VALUES ZEROS.

       PROCEDURE DIVISION.
      *****AREA DE COMANDOS 12 - 72
           ACCEPT WRK-DATA FROM DATE YYYYMMDD.
           DISPLAY 'DATA...' WRK-DIA '/' WRK-MES'/' WRK-ANO.
           STOP RUN.
