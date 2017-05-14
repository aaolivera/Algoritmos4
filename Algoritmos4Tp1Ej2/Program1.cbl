       identification division.
       program-id. Program1.

       environment division.
       configuration section.

       input-output section.
		file-control.
		   select CONSOR-1
		   assign to disk "C:\CONSOR-1.txt"
           organization is line sequential
           file status is fs-CONSOR-1.
           
           select CONSOR-2
		   assign to disk "C:\CONSOR-2.txt"
           organization is line sequential
           file status is fs-CONSOR-2.
           
           select CONSOR-3
		   assign to disk "C:\CONSOR-3.txt"
           organization is line sequential
           file status is fs-CONSOR-3.
                                   
           select ORDENAR ASSIGN TO SORT
           file status is fs-ORDENAR.
           
       data division.
       file section.
		fd CONSOR-1.
		01 REG_CONSOR-1.
		     03 CUIT-CO pic 9(15).
		     03 FECHA-ALTA.
               06 ANIO pic x(4).
               06 FILL pic x(6).
             03 FECHA-BAJA pic x(10).
             03 ESTADO_NUM pic 9(2).
             03 NOMBRE-CONSORCIO pic x(30).
             03 TEL pic x(15).
             03 DIR pic x(30).
        
        fd CONSOR-2.
		01 REG_CONSOR-2.
		     03 CUIT-CO pic 9(15).
		     03 FECHA-ALTA.
               06 ANIO pic x(4).
               06 FILL pic x(6).
             03 FECHA-BAJA pic x(10).
             03 ESTADO_NUM pic 9(2).
             03 NOMBRE-CONSORCIO pic x(30).
             03 TEL pic x(15).
             03 DIR pic x(30).
             
        fd CONSOR-3.
		01 REG_CONSOR-3.
		     03 CUIT-CO pic 9(15).
		     03 FECHA-ALTA.
               06 ANIO pic x(4).
               06 FILL pic x(6).
             03 FECHA-BAJA pic x(10).
             03 ESTADO_NUM pic 9(2).
             03 NOMBRE-CONSORCIO pic x(30).
             03 TEL pic x(15).
             03 DIR pic x(30).
          
        sd ORDENAR.
		01 REG_ORDENAR.
		     03 ANIO pic x(4).
             03 ESTADO_NUM pic 9(2).
             
       working-storage section.
       01 fs-CONSOR-1 pic xx.
		     88 ok-CONSOR-1 value "00".
		     88 eof-CONSOR-1 value "10".
       01 fs-CONSOR-2 pic xx.
		     88 ok-CONSOR-2 value "00".
		     88 eof-CONSOR-2 value "10".
       01 fs-CONSOR-3 pic xx.
		     88 ok-CONSOR-3 value "00".
		     88 eof-CONSOR-3 value "10".
       01 fs-ORDENAR pic xx.
		     88 ok-ORDENAR value "00".
		     88 eof-ORDENAR value "10".
       01 REG_ORDENAR_ANT.
		     03 ANIO pic x(4).
             03 ESTADO_NUM pic 9(2).
       01 CONTADOR pic 999.
       procedure division.
       
       sort ORDENAR
           ON ASCENDING ANIO of ORDENAR
           ON ASCENDING ESTADO_NUM of ORDENAR
       input procedure is entrada
       output procedure is salida.
       
       
       stop run.
       
       salida.
           display "Anio      Estado       Cantidad".
           RETURN ORDENAR.
           perform procesarOrdenado until eof-ORDENAR.
       
       procesarOrdenado.
           move REG_ORDENAR to REG_ORDENAR_ANT.
           move 0 to CONTADOR.
           perform contarEstadoPorAnio until eof-ORDENAR or 
           ANIO of REG_ORDENAR <> ANIO of REG_ORDENAR_ANT or
           ESTADO_NUM of REG_ORDENAR <> ESTADO_NUM of REG_ORDENAR_ANT.
           display ANIO of REG_ORDENAR_ANT "      "
           ESTADO_NUM of REG_ORDENAR_ANT "           " CONTADOR.
       
       contarEstadoPorAnio.
           move REG_ORDENAR to REG_ORDENAR_ANT.
           add 1 to CONTADOR.
           RETURN ORDENAR.

       entrada.
           open input CONSOR-1.
           open input CONSOR-2.
           open input CONSOR-3.
           
           read CONSOR-1.
           read CONSOR-2.
           read CONSOR-3.

           perform procesarConsor1 until eof-CONSOR-1.
           perform procesarConsor2 until eof-CONSOR-2.
           perform procesarConsor3 until eof-CONSOR-3.
           
           close CONSOR-1.
           close CONSOR-2.
           close CONSOR-3.
           
       procesarConsor1.
           move ANIO of REG_CONSOR-1 to ANIO of REG_ORDENAR.
           move ESTADO_NUM of REG_CONSOR-1 to ESTADO_NUM of REG_ORDENAR.
           release REG_ORDENAR.
           read CONSOR-1.
       
       procesarConsor2.
           move ANIO of REG_CONSOR-2 to ANIO of REG_ORDENAR.
           move ESTADO_NUM of REG_CONSOR-2 to ESTADO_NUM of REG_ORDENAR.
           release REG_ORDENAR.
           read CONSOR-2.
       
       procesarConsor3.
           move ANIO of REG_CONSOR-3 to ANIO of REG_ORDENAR.
           move ESTADO_NUM of REG_CONSOR-3 to ESTADO_NUM of REG_ORDENAR.
           release REG_ORDENAR.
           read CONSOR-3.
          
       end program Program1.