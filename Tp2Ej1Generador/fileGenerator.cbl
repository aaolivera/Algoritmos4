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
               access mode is sequential
               RECORD KEY REG_KEY
               FILE STATUS IS fs-CUITPROVOUT.

       DATA DIVISION.

       FILE SECTION.
            fd CUITPROV.
             01 REG_CUITPROV.
                 03 CUIT-CONS pic 9(15).
                 03 COD-PROV pic 9(8).
                 03 FECHA-ALTA.
                   06 ANIO pic x(4).
                   06 FILL pic x(6).

           fd CUITPROVOUT.
             01 REG_CUITPROVOUT.
                 03 REG_KEY.
				   06 CUIT-CONS-OUT pic 9(15).
				   06 COD-PROV-OUT pic 9(08).
				 03 FECHA-ALTA.
					06 ANIO pic x(4).
					06 FILL pic x(6).

       WORKING-STORAGE SECTION.
       01 fs-CUITPROV pic xx.
             88 ok-CUITPROV value "00".
             88 eof-CUITPROV value "10".
        01 fs-CUITPROVOUT pic xx.
             88 ok-CUITPROVOUT value "00".
             88 eof-CUITPROVOUT value "10".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN input CUITPROV.
           OPEN OUTPUT CUITPROVOUT.

           READ CUITPROV
           perform grabar until eof-CUITPROV

           CLOSE CUITPROV.
           CLOSE CUITPROVOUT.

           STOP RUN.

       grabar.
           DISPLAY REG_CUITPROV.
           READ CUITPROV.
           MOVE REG_CUITPROV to REG_CUITPROVOUT.

           WRITE REG_CUITPROVOUT.

       END PROGRAM PGM.
