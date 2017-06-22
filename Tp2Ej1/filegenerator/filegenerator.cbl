      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM.

       environment division.
       configuration section.

       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           select CUITPROV
               assign to disk "CUITPROV.INFO"
               organization is line sequential
               FILE STATUS IS fs-CUITPROV.

           select CUITPROVOUT
               assign to disk "CUITPROV.OUT"
               ORGANIZATION INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY COD-PROV-OUT
               ALTERNATE RECORD KEY CUIT-CONS-OUT WITH DUPLICATES
               FILE STATUS IS fs-CUITPROVOUT.

           select PROV
               assign to disk "PROV.INFO"
               organization is line sequential
               FILE STATUS IS fs-PROV.

           select PROVOUT
               assign to disk "PROV.OUT"
               ORGANIZATION INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY COD-PROV-OUT
               FILE STATUS IS fs-PROV-OUT.
       DATA DIVISION.

       FILE SECTION.
           fd CUITPROV.
               01 REG_CUITPROV.
                   03 CUIT-CONS pic 9(15).
                   03 COD-PROV pic 9(08).
                   03 FECHA-ALTA.
                       06 ANIO pic x(4).
                       06 FILL pic x(4).

           fd CUITPROVOUT.
               01 REG_CUITPROVOUT.
                   03 CUIT-CONS-OUT pic 9(15).
                   03 COD-PROV-OUT pic 9(08).
                   03 FECHA-ALTA-OUT.
                       06 ANIO pic x(4).
                       06 FILL pic x(6).
           fd PROV.
               01 REG_PROV.
                   03 COD-PROV pic 9(8).
                   03 DIR pic 9(30).
                   03 TEL pic 9(15).
                   03 RUBRO pic X(4).
                   03 DESC-RUBRO pic X(15).
                   03 FECHA-ALTA pic 9(8).
                   03 CANT-CONS-ASIG pic 9(4).

           fd PROVOUT.
               01 REG_PROV_OUT.
                   03 PROV-OUT pic 9(8).
                   03 DIR-OUT pic 9(30).
                   03 TEL-OUT pic 9(15).
                   03 RUBRO-OUT pic X(4).
                   03 DESC-RUBRO-OUT pic X(15).
                   03 FECHA-ALTA-OUT pic 9(8).
                   03 CANT-CONS-ASIG-OUT pic 9(4).


       WORKING-STORAGE SECTION.
           01 fs-CUITPROV pic xx.
               88 ok-CUITPROV value "00".
               88 eof-CUITPROV value "10".
           01 fs-CUITPROVOUT pic xx.
               88 ok-CUITPROVOUT value "00".
               88 eof-CUITPROVOUT value "10".
           01 fs-PROV pic xx.
               88 ok-PROV value "00".
               88 eof-PROV value "10".
           01 fs-PROV-OUT pic xx.
               88 ok-PROV-OUT value "00".
               88 eof-PROV-OUT value "10".


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN input CUITPROV.
           OPEN OUTPUT CUITPROVOUT.

           DISPLAY "ARCHIVO DE CONSORCIOS"

           READ CUITPROV
           perform grabar_CUITPROVOUT until eof-CUITPROV

           CLOSE CUITPROV.
           CLOSE CUITPROVOUT.

           OPEN input PROV.
           OPEN OUTPUT PROVOUT.

           DISPLAY "ARCHIVO DE PROVEEDORES"
           READ PROV.
           PERFORM grabar_PROVOUT until eof-PROV.

           CLOSE PROV.
           CLOSE PROVOUT.

           STOP RUN.

       grabar_CUITPROVOUT.

           MOVE CUIT-CONS of REG_CUITPROV to CUIT-CONS-OUT of
           REG_CUITPROVOUT.

           MOVE COD-PROV of REG_CUITPROV to COD-PROV-OUT of
           REG_CUITPROVOUT.

           MOVE FECHA-ALTA of REG_CUITPROV to FECHA-ALTA-OUT of
           REG_CUITPROVOUT.

           DISPLAY REG_CUITPROV.

           WRITE REG_CUITPROVOUT.

           READ CUITPROV.


       grabar_PROVOUT.

           MOVE REG_PROV to REG_PROV_OUT.

           DISPLAY REG_PROV_OUT.

           WRITE REG_PROV_OUT.

           READ PROV.

       END PROGRAM PGM.
