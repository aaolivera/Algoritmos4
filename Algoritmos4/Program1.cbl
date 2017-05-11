		identification division.
		program-id. Program1.
		
		ENVIRONMENT division.
		configuration section.

		input-output section.
		file-control.
		     select CONSOR-1
		     assign to disk "C:\CONSOR-1.txt".
             
	         select CUENTAS
		     assign to disk "C:\CUENTAS.txt".
             
             select ESTADO
		     assign to disk "C:\ESTADO.txt".
             
		DATA division.
		file section.
		fd CONSOR-1.
		01 REG_CONSOR.
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
		01 fs-arch pic xx.
		     88 ok-arch value "00".
		     88 eof-arch value "10".        01 exitval pic x.
        
		PROCEDURE division.
			open input ESTADO.
			perform leerarch.
			perform cargar until eof-arch or indice > 30.
			close ESTADO.
			accept exitval.
		cargar .
			move corresponding REG_ESTADO to tablaEstado(indice).
			display tablaEstado(indice).
			add 1 to indice.
			perform leerarch.
            
		leerarch.
			read ESTADO .
			if (not ok-arch)
			    display "ERROR AL LEER : " fs-arch
			end-if.

		    goback.
		end program Program1.
        

