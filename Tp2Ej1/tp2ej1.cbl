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
               FILE STATUS IS fs-CUITPROV.

               SELECT MAESTRO
                   assign to disk "MAESTRO.txt"
                   organization is line sequential
                   file status is fs-MAESTRO.

               select PROV
                   assign to disk "PROV.OUT"
                   ORGANIZATION INDEXED
                   ACCESS MODE is RANDOM
                   RECORD KEY IS COD-PROV of REG_PROV
               FILE STATUS IS fs-PROV.

               SELECT ORDENAR
                       ASSIGN TO "WORK.TMP"
                  FILE status is fs-ORDENAR.
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

           fd MAESTRO.
               01 REG_MAESTRO.
                   03 CUIT-CO pic 9(15).
                   03 FECHA-ALTA pic x(10).
                   03 DESCRIP pic X(15).
                   03 NOMBRE-CONSORCIO pic x(30).
                   03 TEL pic x(15).
                   03 DIR pic x(30).
                   03 NRO-CTA pic 9(8).

           fd PROV.
               01 REG_PROV.
                   03 COD-PROV pic 9(8).
                   03 DIR pic 9(30).
                   03 TEL pic 9(15).
                   03 RUBRO pic X(4).
                   03 DESC-RUBRO pic X(15).
                   03 FECHA-ALTA pic 9(8).
                   03 CANT pic 9(3).

           SD ORDENAR.
               01 REG_ORDENAR.
                   03 RUBRO pic x(4).
                   03 COD-PROV pic 9(8).
                   03 CUIT-CONS pic 9(15).
                   03 NOMBRE-CONSORCIO pic x(30).
                   03 TEL pic x(15).
                   03 DIR pic x(30).

       WORKING-STORAGE SECTION.
           01 fs-CUITPROV pic xx.
               88 ok-CUITPROV value "00".
               88 eof-CUITPROV value "10".

           01 fs-MAESTRO pic xx.
		         88 ok-MAESTRO value "00".
		         88 eof-MAESTRO value "10".

           01 fs-PROV pic xx.
		         88 ok-PROV value "00".
		         88 eof-PROV value "10".

           01 fs-ORDENAR pic xx.
		         88 ok-ORDENAR value "00".
		         88 eof-ORDENAR value "10".

           01 tablaConteo.
               03 COUNTER pic 9(08) VALUE 0 OCCURS 99999999 TIMES.

           01 COUNTERAUX pic 9(08).
           01 CONSOR_ITERATOR pic 9(15) VALUE 85.

           01 WS-YYYY-MM-DD pic 9(8).
           01 PAGINAS pic 99.
           01 LINEAS pic 9(8).
           01 LINEASAAGREGAR pic 9(8).

           01 FLAG pic 9(1) VALUE 0.

       procedure division.

           OPEN input CUITPROV.
           READ CUITPROV NEXT RECORD.
           PERFORM CONTAR UNTIL eof-CUITPROV.

           CALL  "actualizarProv" USING tablaConteo.
           CLOSE CUITPROV.

           MOVE 1 to PAGINAS.
           MOVE 0 to LINEAS.


           SORT ORDENAR
               ON ASCENDING RUBRO of REG_ORDENAR
               ON ASCENDING COD-PROV of REG_ORDENAR
               ON ASCENDING CUIT-CONS of REG_ORDENAR
           INPUT PROCEDURE is ENTRADA
           OUTPUT PROCEDURE is SALIDA.

       STOP RUN.

       CONTAR.


           DISPLAY CUIT-CONS of REG_CUITPROV.
           DISPLAY COD-PROV of REG_CUITPROV.

           MOVE COUNTER(COD-PROV of REG_CUITPROV) TO COUNTERAUX.

           ADD 1 to COUNTERAUX.

           MOVE  COUNTERAUX TO COUNTER(COD-PROV of REG_CUITPROV).
           READ CUITPROV NEXT RECORD.

       ENTRADA.
           OPEN INPUT CUITPROV.

           READ CUITPROV NEXT RECORD.
           PERFORM LOAD_RECORD UNTIL eof-CUITPROV.
           CLOSE CUITPROV.


       SALIDA.
            PERFORM EMITIR_ENCABEZADO.
      *>       RETURN ORDENAR.

       EMITIR_ENCABEZADO.
           accept WS-YYYY-MM-DD from date yyyymmdd.
           display "Fecha: "WS-YYYY-MM-DD"                             "
           "                  Hoja nro "PAGINAS.
           display "                  LISTADO DE PROVEEDORES ASIGNADOS".
           display " ".
           move 3 to LINEAS.

       LOAD_RECORD.
           MOVE COD-PROV of REG_CUITPROV TO COD-PROV of REG_ORDENAR
           MOVE CUIT-CONS of REG_CUITPROV TO CUIT-CONS of REG_ORDENAR

           OPEN INPUT MAESTRO.
           READ MAESTRO.
           PERFORM BUSCAR_DATA_MAESTRO UNTIL (FLAG is EQUAL 1
               OR eof-MAESTRO).
           MOVE 0 to FLAG.
           CLOSE MAESTRO.

           DISPLAY "-------------".
           PERFORM BUSCAR_DATA_PROV.
           DISPLAY "REG_ORDENAR: " REG_ORDENAR.
           READ CUITPROV NEXT RECORD.


       BUSCAR_DATA_MAESTRO.
           if (CUIT-CONS of REG_CUITPROV) =
               (CUIT-CO of REG_MAESTRO)
               MOVE NOMBRE-CONSORCIO of REG_MAESTRO TO NOMBRE-CONSORCIO
                   of REG_ORDENAR
               MOVE TEL of REG_MAESTRO to TEL of REG_ORDENAR
               MOVE DIR OF REG_MAESTRO to DIR of REG_ORDENAR
               MOVE 1 TO FLAG.
           READ MAESTRO.

       BUSCAR_DATA_PROV.

           OPEN INPUT PROV.
           MOVE COD-PROV of REG_CUITPROV TO COD-PROV of REG_PROV.

           READ PROV.
           MOVE RUBRO of REG_PROV to RUBRO of REG_ORDENAR.
           CLOSE PROV.

       end program tp2ej1.
