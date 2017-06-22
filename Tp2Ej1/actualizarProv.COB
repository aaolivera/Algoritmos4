      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. actualizarProv.

       environment division.
       configuration section.

       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           select PROV
               assign to disk "PROV.OUT"
               organization INDEXED ACCESS DYNAMIC
               RECORD KEY COD-PROV
               FILE STATUS IS fs-PROV.

           DATA DIVISION.

           FILE SECTION.
           fd PROV.
               01 REG_PROV.
                   03 COD-PROV pic 9(8).
                   03 DIR pic 9(30).
                   03 TEL pic 9(15).
                   03 RUBRO pic X(4).
                   03 DESC-RUBRO pic X(15).
                   03 FECHA-ALTA pic 9(8).
                   03 CANT-CONS-ASIG pic 9(3).


           WORKING-STORAGE SECTION.
           01 fs-PROV pic xx.
               88 ok-PROV value "00".
               88 eof-PROV value "10".

           01 iterator pic 9(8) VALUE 1.

           LINKAGE SECTION.
           01 tablaConteo.
               03 COUNTER pic 9(08) VALUE 0 OCCURS 99999999 TIMES.

       PROCEDURE DIVISION USING tablaConteo.
       MAIN-PROCEDURE.
      *>      DISPLAY "--- PRUEBA PASAJE DE VARIABLES ---"
      *>      DISPLAY COUNTER(2).

      *>      DISPLAY "--- PRUEBA LECTURA PROV---"
      *>      OPEN INPUT PROV.
      *>      READ PROV NEXT RECORD.
      *>      PERFORM leer UNTIL eof-PROV.
      *>      CLOSE PROV.

           DISPLAY "--- ACTUALIZACION DE ARCHIVO ---"

           OPEN I-O PROV.
           PERFORM actualizar UNTIL iterator EQUAL 9.
           CLOSE PROV.

           EXIT PROGRAM.

       leer.
           DISPLAY REG_PROV.
           READ PROV NEXT RECORD.



       actualizar.
           if(COUNTER(iterator) > 0)
               MOVE iterator to COD-PROV of REG_PROV
               READ PROV
                   INVALID KEY DISPLAY "ERROR"
               END-READ

               MOVE COUNTER(iterator) TO CANT-CONS-ASIG of REG_PROV
               DISPLAY REG_PROV
               REWRITE REG_PROV
               END-REWRITE.

           ADD 1 TO iterator.
       END PROGRAM actualizarProv.
