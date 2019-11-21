      ******************************************************************
      * Author: Julio Cesar
      * Date: 11/11/2019
      * Purpose:Fulano tem 1,50 metro e cresce 2 centímetros por ano,
      * enquanto Sicrano tem 1,10 metro e cresce 3 centímetros por ano.
      * Construa o algoritmo que calcule e imprima quantos anos serão necessários para que
      * Sicrano seja maior que Fulano, e quais as respectivas alturas na época.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO03.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 FULANO   PIC 9(1)V9(2) VALUE 1.5.
       01 SICLANO  PIC 9(1)V9(2) VALUE 1.1.
       01 ANOS     PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       00-PRINCIPAL.
           PERFORM 1-INICIAR
           PERFORM 2-PROCESSAR
           PERFORM 3-ENCERRAR
            STOP RUN
           .
       1-INICIAR.
           DISPLAY 'FULANO: '  FULANO
           DISPLAY 'SICLANO: ' SICLANO
           .

       2-PROCESSAR.
           PERFORM UNTIL SICLANO > FULANO
               ADD 1 TO ANOS
               ADD 0.02 TO FULANO
               ADD 0.03 TO SICLANO
           END-PERFORM
           .

       3-ENCERRAR.
           DISPLAY 'APOS ' ANOS ' ANOS A ALTURA DE FULANO SERA DE '
               FULANO ' E A DE SICLANO  ' SICLANO
           .

       END PROGRAM DESAFIO03.
