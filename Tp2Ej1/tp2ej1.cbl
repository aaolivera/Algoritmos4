       identification division.
       program-id. tp2ej1.

       environment division.
       configuration section.

       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               select CUITPROV
                   assign to disk "CUITPROV.OUT"
                   organization INDEXED ACCESS RANDOM
                   RECORD KEY REG_KEY
                   ALTERNATE RECORD KEY COD-PROV WITH DUPLICATES
               FILE STATUS IS fs-CUITPROV.

       data division.


       FILE SECTION.
           fd CUITPROV.
           01 REG_CUITPROV.
               03 REG_KEY.
                   06 CUIT-CONS pic 9(15).
                   06 COD-PROV pic 9(8).
               03 FECHA-ALTA.
               06 ANIO pic x(4).
               06 FILL pic x(6).

       WORKING-STORAGE SECTION.
           01 fs-CUITPROV pic xx.
               88 ok-CUITPROV value "00".
               88 eof-CUITPROV value "10".

           01 tablaConteo.
               03 COUNTER pic 9(08) VALUE 0 OCCURS 99999999 TIMES.

           01 COUNTERAUX pic 9(08).
           01 CONSOR_ITERATOR pic 9(15) VALUE 85.

           01 WS-YYYY-MM-DD pic 9(8).
           01 PAGINAS pic 99.
           01 LINEAS pic 9(8).
           01 LINEASAAGREGAR pic 9(8).

       procedure division.

           OPEN input CUITPROV.
           READ CUITPROV NEXT RECORD.
           PERFORM CONTAR UNTIL eof-CUITPROV.

           CALL  "actualizarProv" USING tablaConteo.
           CLOSE CUITPROV.

           MOVE 1 to PAGINAS.
           MOVE 0 to LINEAS.
           PERFORM EMITIR_LISTADO.

       STOP RUN.

       CONTAR.


           DISPLAY CUIT-CONS of REG_CUITPROV.
           DISPLAY COD-PROV of REG_CUITPROV.

           MOVE COUNTER(COD-PROV of REG_CUITPROV) TO COUNTERAUX.

           ADD 1 to COUNTERAUX.

           MOVE  COUNTERAUX TO COUNTER(COD-PROV of REG_CUITPROV).
           READ CUITPROV NEXT RECORD.

       EMITIR_LISTADO.
           PERFORM EMITIR_ENCABEZADO.



       EMITIR_ENCABEZADO.
           accept WS-YYYY-MM-DD from date yyyymmdd.
           display "Fecha: "WS-YYYY-MM-DD"                             "
           "                  Hoja nro "PAGINAS.
           display "                  LISTADO DE PROVEEDORES ASIGNADOS".
           display " ".
           move 3 to LINEAS.

       end program tp2ej1.
