      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRO.

       environment division.
       configuration section.

       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           select PROV
               assign to disk "PROV.OUT"
               organization INDEXED ACCESS DYNAMIC
               RECORD KEY COD-PROV of REG_PROV
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
               
           01 funcion pic x(10).
           
           01 REG_PROV_O.
               03 COD-PROV pic 9(8).
               03 DIR pic 9(30).
               03 TEL pic 9(15).
               03 RUBRO pic X(4).
               03 DESC-RUBRO pic X(15).
               03 FECHA-ALTA pic 9(8).
               03 CANT-CONS-ASIG pic 9(3).
                   
       PROCEDURE DIVISION USING funcion tablaConteo REG_PROV_O.

           if(funcion equal "abr")
               OPEN I-O PROV.
           if(funcion equal "cer")
               CLOSE PROV.
           if(funcion equal "act")
               move 1 to iterator
               PERFORM actualizar UNTIL iterator EQUAL 99999999.
           if(funcion equal "lee")
               move REG_PROV_O to REG_PROV
               
               READ PROV
                   INVALID KEY DISPLAY "ERROR"
               END-READ
               move REG_PROV to REG_PROV_O.
       EXIT PROGRAM.

       actualizar.
           if(COUNTER(iterator) > 0)
               MOVE iterator to COD-PROV of REG_PROV
               READ PROV
                   INVALID KEY DISPLAY "ERROR"
               END-READ

               MOVE COUNTER(iterator) TO CANT-CONS-ASIG of REG_PROV
               DISPLAY "--"REG_PROV
               REWRITE REG_PROV
               END-REWRITE.

           ADD 1 TO iterator.
       END PROGRAM PRO.
