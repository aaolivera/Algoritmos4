       identification division.
       program-id. tp2ej1.

       environment division.
       configuration section.

       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               select CUITPROV
                   assign to disk "CUITPROV.OUT"
                   organization INDEXED
                   access mode is sequential
                   RECORD KEY REG_KEY
                   FILE STATUS IS fs-CUITPROV.

               SELECT MAESTRO
                   assign to disk "MAESTRO.txt"
                   organization is line sequential
                   file status is fs-MAESTRO.

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

           SD ORDENAR.
               01 REG_ORDENAR.
                   03 RUBRO pic x(4).
                   03 COD-PROV pic 9(8).
                   03 DESC pic X(15).
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
           
           01 REG_PROV.
                   03 COD-PROV pic 9(8).
                   03 DIR pic 9(30).
                   03 TEL pic 9(15).
                   03 RUBRO pic X(4).
                   03 DESC-RUBRO pic X(15).
                   03 FECHA-ALTA pic 9(8).
                   03 CANT pic 9(3).
               
           
           01 COUNTERAUX pic 9(08).
           01 WS-YYYY-MM-DD pic 9(8).
           01 PAGINAS pic 99.
           01 LINEAS pic 9(8).
           01 RUBRO_ACT pic x(4).
           01 COD-PROV_ACT pic 9(8).
           01 CONTADOR_PROV_POR_RUBRO pic 9(2).
           01 CONTADOR_TOTAL_RUBRO pic 9(2).

       procedure division.
           display "|--- Iniciando Tp2 ---|".
           
           perform ContarConsorciosPorProveedor.
           
           CALL "PRO" USING "abr".
           CALL "PRO" USING "act" tablaConteo.
           
           perform GenerarListado.

           CALL "PRO" USING "cer".
       STOP RUN.
       
       ContarConsorciosPorProveedor.
           display "--- Procesando archivo CUITPROV ---".
           OPEN input CUITPROV.
           READ CUITPROV NEXT RECORD.
           PERFORM CONTAR UNTIL eof-CUITPROV.
           CLOSE CUITPROV.
           display "--- Fin CUITPROV ---".
           
       CONTAR.
           display "--CUIT-CONS "CUIT-CONS of REG_CUITPROV" COD-PROV "
           COD-PROV of REG_CUITPROV" ".

           MOVE COUNTER(COD-PROV of REG_CUITPROV) TO COUNTERAUX.

           ADD 1 to COUNTERAUX.

           MOVE  COUNTERAUX TO COUNTER(COD-PROV of REG_CUITPROV).
           READ CUITPROV NEXT RECORD.

       GenerarListado.
           display "---------------------------------------".
           display "--- Generación de listado por rubro ---".
           display "---------------------------------------"..
           MOVE 1 to PAGINAS.
           MOVE 0 to LINEAS.
           SORT ORDENAR
               ON ASCENDING RUBRO of REG_ORDENAR
               ON ASCENDING COD-PROV of REG_ORDENAR
               ON ASCENDING CUIT-CONS of REG_ORDENAR
           INPUT PROCEDURE is ENTRADA
           OUTPUT PROCEDURE is SALIDA.
           
       ENTRADA.
           OPEN INPUT CUITPROV.
           OPEN INPUT MAESTRO.
           
           READ MAESTRO.
           READ CUITPROV.
           PERFORM LOAD_MAE UNTIL eof-MAESTRO or eof-CUITPROV.
           CLOSE CUITPROV.
           CLOSE MAESTRO.
           
       LOAD_MAE.
           PERFORM LOAD_CUITPROV UNTIL eof-CUITPROV or
           (CUIT-CONS of REG_CUITPROV) > (CUIT-CO of REG_MAESTRO)
       
           READ MAESTRO.
       
       LOAD_CUITPROV.
           move COD-PROV of REG_CUITPROV to REG_PROV.
           CALL "PRO" USING "lee" null REG_PROV.
       
           MOVE CUIT-CO of REG_MAESTRO TO CUIT-CONS of REG_ORDENAR
           MOVE NOMBRE-CONSORCIO of REG_MAESTRO TO NOMBRE-CONSORCIO
                   of REG_ORDENAR
           MOVE TEL of REG_MAESTRO to TEL of REG_ORDENAR
           MOVE DIR OF REG_MAESTRO to DIR of REG_ORDENAR

           MOVE DESC-RUBRO of REG_PROV to DESC of REG_ORDENAR.
           MOVE RUBRO of REG_PROV to RUBRO of REG_ORDENAR.
               
           MOVE COD-PROV of REG_CUITPROV TO COD-PROV of REG_ORDENAR
                                                                                
           release REG_ORDENAR
           READ CUITPROV NEXT RECORD.
       
           
       SALIDA.
           RETURN ORDENAR.
               
           PERFORM EMITIR_ENCABEZADO.
           PERFORM EMITIR_ENCABEZADO_RUBRO.
           
           MOVE 1 TO CONTADOR_TOTAL_RUBRO.
           MOVE 1 TO CONTADOR_PROV_POR_RUBRO.
           
           MOVE RUBRO of REG_ORDENAR to RUBRO_ACT.
           move COD-PROV of REG_ORDENAR to COD-PROV_ACT 
           
           PERFORM PROCESAR_ORDENADO until eof-ORDENAR.
           PERFORM EMITIR_TOTAL_PROOVEEDORES_POR_RUBRO.
           PERFORM EMITIR_TOTAL_RUBRO.

       PROCESAR_ORDENADO.
           IF RUBRO_ACT IS EQUAL TO RUBRO of REG_ORDENAR
               AND COD-PROV_ACT is not equal to COD-PROV of REG_ORDENAR
                   ADD 1 TO CONTADOR_PROV_POR_RUBRO
                   move COD-PROV of REG_ORDENAR to COD-PROV_ACT.
           
           IF RUBRO_ACT IS NOT EQUAL TO RUBRO of REG_ORDENAR
               PERFORM EMITIR_TOTAL_PROOVEEDORES_POR_RUBRO
               PERFORM EMITIR_TOTAL_RUBRO
               DISPLAY " "
               PERFORM EMITIR_ENCABEZADO_RUBRO
               MOVE 0 TO CONTADOR_PROV_POR_RUBRO
               ADD 1 TO CONTADOR_TOTAL_RUBRO
               MOVE RUBRO of REG_ORDENAR to RUBRO_ACT.

           DISPLAY COD-PROV of REG_ORDENAR"   "
                   CUIT-CONS of REG_ORDENAR " "
                   NOMBRE-CONSORCIO of REG_ORDENAR " "
                   TEL OF REG_ORDENAR " "
                   DIR of REG_ORDENAR.
           ADD 1 TO LINEAS.
           
           RETURN ORDENAR RECORD INTO REG_ORDENAR.

       VALIDAR_PAGINA.
           if(LINEAS + 1 > 60)
               add 1 to PAGINAS
               perform EMITIR_ENCABEZADO.
               
       EMITIR_ENCABEZADO.
           accept WS-YYYY-MM-DD from date yyyymmdd.
           display "Fecha: "WS-YYYY-MM-DD"                             "
           "                  Hoja nro "PAGINAS.
           display "                  LISTADO DE PROVEEDORES ASIGNADOS".
           display " ".
           move 3 to LINEAS.


       EMITIR_ENCABEZADO_RUBRO.
       DISPLAY "RUBRO: " RUBRO of REG_ORDENAR "  DESCRIPCION: "
           DESC OF REG_ORDENAR.
       DISPLAY
               "COD_PROV   CUIT-CONSORCIO  NOMBRE-CONSORCIO            "
               "   TEL-CONS        DIR-CONS".
       move 3 to LINEAS.
       EMITIR_TOTAL_PROOVEEDORES_POR_RUBRO.
       DISPLAY "TOTAL DE PROVEEDORES POR RUBRO: "
               CONTADOR_PROV_POR_RUBRO.
       move 1 to LINEAS.
       EMITIR_TOTAL_RUBRO.
       DISPLAY "TOTAL RUBROS: " CONTADOR_TOTAL_RUBRO.
       move 1 to LINEAS.