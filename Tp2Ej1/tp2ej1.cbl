       identification division.
       program-id. tp2ej1.

       environment division.
       configuration section.

       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               select CUITPROV
                   assign to disk "C:\CUITPROV.OUT"
                   organization INDEXED ACCESS DYNAMIC
                   RECORD KEY COD-PROV
                   ALTERNATE RECORD KEY CUIT-CONS WITH DUPLICATES
               FILE STATUS IS fs-CUITPROV.

       data division.


       FILE SECTION.
           fd CUITPROV.
           01 REG_CUITPROV.
               03 CUIT-CONS pic 9(15).
               03 COD-PROV pic 9(8).
               03 FECHA-ALTA.
               06 ANIO pic x(4).
               06 FILL pic x(6).

       WORKING-STORAGE SECTION.
           01 fs-CUITPROV pic xx.
               88 ok-CUITPROV value "00".
               88 eof-CUITPROV value "10".

           01 tablaConteo.
               03 COUNTER OCCURS 99999999 TIMES pic 9(08).
           01 COUNTERAUX pic 9(10).

       procedure division.
           OPEN INPUT CUITPROV.

           CALL 'actualizarProv' USING tablaConteo.

           READ CUITPROV NEXT RECORD.
           PERFORM contar UNTIL eof-CUITPROV.

           DISPLAY COUNTER(3).
           CALL  "actualizarProv" USING tablaConteo.

           CLOSE CUITPROV.
       STOP RUN.

       contar.
           DISPLAY COD-PROV of REG_CUITPROV.
           DISPLAY CUIT-CONS of REG_CUITPROV.
           DISPLAY FECHA-ALTA of REG_CUITPROV.
      *>      MOVE COUNTER(COD-PROV of REG_CUITPROV) TO COUNTERAUX.

           ADD 1 to COUNTERAUX.
           DISPLAY COUNTERAUX.

      *>      MOVE  COUNTERAUX TO COUNTER(COD-PROV of REG_CUITPROV).
           READ CUITPROV NEXT RECORD.

      *>  actualizarProv.
      *>      OPEN OUTPUT PROV.
      *>      MOVE 1 TO i
      *>      PERFORM actualizar UNTIL i == 10000000.
      *>      CLOSE PROV.
      *>  actualizar.
      *>      if(tablaconteo(i) > 0)
      *>          READ PROV RECORD KEY IS i
      *>          MOVE tablaConteo(i) TO PRO_REG_CANT_CONS
      *>          REWRITE PROV_REG.
      *>      ADD 1 TO i.

       end program tp2ej1.
