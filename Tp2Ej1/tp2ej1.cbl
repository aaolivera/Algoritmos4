       identification division.
       program-id. tp2ej1.

       environment division.
       configuration section.

       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               select CUITPROV
                   assign to disk "CUITPROV.OUT"
                   organization INDEXED ACCESS DYNAMIC
                   RECORD KEY COD-PROV
                   ALTERNATE RECORD KEY CUIT-CONS WITH DUPLICATES
               FILE STATUS IS fs-CUITPROV.

       data division.


       FILE SECTION.
           fd CUITPROV.
           01 REG_CUITPROV.
               03 CUIT-CONS pic 9(15).
               03 COD-PROV pic 9(08).
               03 FECHA-ALTA.
               06 ANIO pic x(4).
               06 FILL pic x(6).

       WORKING-STORAGE SECTION.
           01 fs-CUITPROV pic xx.
               88 ok-CUITPROV value "00".
               88 eof-CUITPROV value "10".

           01 CLAVE pic 9(08).
           01 COUNTER pic 9(10).
           01 eof-file pic 9(1).
       procedure division.
           OPEN INPUT CUITPROV.

           MOVE 3 to CLAVE.
           MOVE 0 to COUNTER.

           START CUITPROV
               KEY IS GREATER THAN CLAVE
               INVALID KEY DISPLAY "error no hay clave".

           READ CUITPROV NEXT RECORD
               AT END SET eof-CUITPROV TO TRUE
           END-READ.

      *>      PERFORM UNTIL eof-CUITPROV
      *>          READ CUITPROV RECORD
      *>              add 1 to COUNTER
      *>          END-READ
      *>      END PERFORM.

           CLOSE CUITPROV.
       STOP RUN.

       contar.
           add 1 to COUNTER.

       end program tp2ej1.
