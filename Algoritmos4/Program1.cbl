		identification division.
		program-id. Program1.
		
		ENVIRONMENT division.
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
             
	         select CUENTAS
		     assign to disk "C:\CUENTAS.txt"
             organization is line sequential
             file status is fs-CUENTAS.
             
             select ESTADO
		     assign to disk "C:\ESTADO.txt"
             organization is line sequential
             file status is fs-ESTADO.
             
		DATA division.
		file section.
		fd CONSOR-1.
		01 REG_CONSOR-1.
		     03 CUIT-CO pic 9(15).
		     03 FECHA-ALTA pic x(10).
             03 FECHA-BAJA pic x(10).
             03 ESTADO_NUM pic 9(2).
             03 NOMBRE-CONSORCIO pic x(30).
             03 TEL pic x(15).
             03 DIR pic x(30).
        
        fd CONSOR-2.
		01 REG_CONSOR-2.
		     03 CUIT-CO pic 9(15).
		     03 FECHA-ALTA pic x(10).
             03 FECHA-BAJA pic x(10).
             03 ESTADO_NUM pic 9(2).
             03 NOMBRE-CONSORCIO pic x(30).
             03 TEL pic x(15).
             03 DIR pic x(30).
             
        fd CONSOR-3.
		01 REG_CONSOR-3.
		     03 CUIT-CO pic 9(15).
		     03 FECHA-ALTA pic x(10).
             03 FECHA-BAJA pic x(10).
             03 ESTADO_NUM pic 9(2).
             03 NOMBRE-CONSORCIO pic x(30).
             03 TEL pic x(15).
             03 DIR pic x(30).
             
        fd CUENTAS.
		01 REG_CUENTAS.
		     03 CUIT-CO pic 9(15).
		     03 NRO-CTA pic 9(8).
             03 FECHA-ALTA pic x(10).
             03 ENTIDAD pic 9(3).
             03 SUCURSAL pic 9(3).
             
        fd ESTADO.
		01 REG_ESTADO.
		     03 ESTADO_NUM pic 9(2).
			 03 DESCRIP pic X(15).
             
		working-storage section.
		01 tablaEstado occurs 30 times.
			03 ESTADO_NUM pic 9(2).
			03 DESCRIP pic X(15).
            
        01 indice pic 99.
		01 fs-CONSOR-1 pic xx.
		     88 ok-CONSOR-1 value "00".
		     88 eof-CONSOR-1 value "10".
        01 fs-CONSOR-2 pic xx.
		     88 ok-CONSOR-2 value "00".
		     88 eof-CONSOR-2 value "10".
        01 fs-CONSOR-3 pic xx.
		     88 ok-CONSOR-3 value "00".
		     88 eof-CONSOR-3 value "10".
        01 fs-CUENTAS pic xx.
		     88 ok-CUENTAS value "00".
		     88 eof-CUENTAS value "10".
        01 fs-ESTADO pic xx.
		     88 ok-ESTADO value "00".
		     88 eof-ESTADO value "10".
        01 exitval pic x.
        
        01 NOVEDADES pic 9(4).
        01 BAJAS pic 9(4).
        01 REG_MENOR pic 9(15).
             
        01 REG_ANT.
		     03 CUIT-CO pic 9(15).
		     03 FECHA-ALTA pic x(10).
             03 FECHA-BAJA pic x(10).
             03 ESTADO_NUM pic 9(2).
             03 NOMBRE-CONSORCIO pic x(30).
             03 TEL pic x(15).
             03 DIR pic x(30). 
        01 REG_C_ANT.
		     03 CUIT-CO pic 9(15).
		     03 NRO-CTA pic 9(8).
             03 FECHA-ALTA pic x(10).
             03 ENTIDAD pic 9(3).
             03 SUCURSAL pic 9(3).
        
        01 WS-YYYY-MM-DD pic 9(8).
        
		PROCEDURE division.
           perform cargarEstado.
           open input CONSOR-1.
           open input CONSOR-2.
           open input CONSOR-3.
           open input CUENTAS.
           
           read CONSOR-1
           read CONSOR-2
           read CONSOR-3
           read CUENTAS
           
           move 0 to BAJAS.
           move 0 to NOVEDADES.
           perform imprimirEncabezado.
           
           
           perform procesarConsorcios until eof-CUENTAS and eof-CONSOR-1
           and eof-CONSOR-2 and eof-CONSOR-3.
           
           display "Total de Consorcios dados de baja: " BAJAS.
           close CONSOR-1.
           close CONSOR-2.
           close CONSOR-3.
           close CUENTAS.
        stop run.
        
        imprimirEncabezado.
           accept WS-YYYY-MM-DD from date yyyymmdd.
           display "Fecha: " WS-YYYY-MM-DD.
           display "            LISTADO DE CONSORCIOS DE BAJA".
        
        procesarConsorcios.
           perform determinarMenor.
           move 0 to NOVEDADES.
           perform procesarCuentas until eof-CUENTAS or (REG_MENOR <> 
                                               CUIT-CO of REG_CUENTAS).
                                               
           perform procesarConsor1 until eof-CONSOR-1 or (REG_MENOR <> 
                                               CUIT-CO of REG_CONSOR-1).                             
           
           perform procesarConsor2 until eof-CONSOR-2 or (REG_MENOR <> 
                                               CUIT-CO of REG_CONSOR-2).
                                               
           perform procesarConsor3 until eof-CONSOR-3 or (REG_MENOR <> 
                                               CUIT-CO of REG_CONSOR-3).
                                               
           perform generarMaestro.
           perform listarBajas.
             
        listarBajas.
           if ESTADO_NUM of REG_ANT = 2
               add 1 to BAJAS
               display "baja".
        
        generarMaestro.
           if CUIT-CO of REG_C_ANT <> REG_MENOR and 
           ESTADO_NUM of REG_ANT <> 2
               display "registro sin cuenta".
               
           if CUIT-CO of REG_C_ANT = REG_MENOR and 
           ESTADO_NUM of REG_ANT <> 2
               display "registro con cuenta".
           
                                                                                    
        procesarConsor1.
           add 1 to NOVEDADES.
           move REG_CONSOR-1 to REG_ANT.
           read CONSOR-1.
           
        procesarConsor2.
           add 1 to NOVEDADES.
           move REG_CONSOR-2 to REG_ANT.
           read CONSOR-2.
           
        procesarConsor3.
           add 1 to NOVEDADES.
           move REG_CONSOR-3 to REG_ANT.
           read CONSOR-3.
        
        procesarCuentas.
           move REG_CUENTAS to REG_C_ANT.
           read CUENTAS.
        
        determinarMenor.
           move 999999999999999 to REG_MENOR.
           
           if REG_MENOR >= CUIT-CO of REG_CONSOR-1 
           and not eof-CONSOR-1
               move CUIT-CO of REG_CONSOR-1 to REG_MENOR
           
           if REG_MENOR >= CUIT-CO of REG_CONSOR-2 
           and not eof-CONSOR-2
               move CUIT-CO of REG_CONSOR-2 to REG_MENOR.
               
           if REG_MENOR >= CUIT-CO of REG_CONSOR-3
           and not eof-CONSOR-3
               move CUIT-CO of REG_CONSOR-3 to REG_MENOR.
        
        cargarEstado.
			open input ESTADO.
            read ESTADO.
            move 1 to indice.
            perform leerESTADO until eof-ESTADO or indice > 30.
            close ESTADO.
            
        leerESTADO.
            move corresponding REG_ESTADO to tablaEstado(indice).
            add 1 to indice.
            read ESTADO.

		end program Program1.
        

