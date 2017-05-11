		identification division.
		program-id. Program1.
		
		ENVIRONMENT division.
		configuration section.

		input-output section.
		file-control.
		     select CONSOR-1
		     assign to disk "C:\CONSOR-1.txt"
             file status is fs-CONSOR.
             
	         select CUENTAS
		     assign to disk "C:\CUENTAS.txt"
             file status is fs-CUENTAS.
             
             select ESTADO
		     assign to disk "C:\ESTADO.txt"
             organization is line sequential
             file status is fs-ESTADO.
             
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
		01 fs-CONSOR pic xx.
		     88 ok-CONSOR value "00".
		     88 eof-CONSOR value "10".
        01 fs-CUENTAS pic xx.
		     88 ok-CUENTAS value "00".
		     88 eof-CUENTAS value "10".
        01 fs-ESTADO pic xx.
		     88 ok-ESTADO value "00".
		     88 eof-ESTADO value "10".
        01 exitval pic x.
        
		PROCEDURE division.
			open input ESTADO.
            read ESTADO.
            move 1 to indice.
            perform cargarESTADO until eof-ESTADO or indice > 30.
            close ESTADO.
            accept exitval.
            
        cargarESTADO.
            move corresponding REG_ESTADO to tablaEstado(indice).
            add 1 to indice.
            read ESTADO.

		end program Program1.
        

