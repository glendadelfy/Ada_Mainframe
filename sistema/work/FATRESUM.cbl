       IDENTIFICATION DIVISION.
       PROGRAM-ID.  FATRESUM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-DIVERG ASSIGN TO FATURA
                  FILE STATUS IS WS-FS-DIVERG.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-DIVERG
           RECORDING MODE IS F.
           COPY CPDIVER.
       WORKING-STORAGE SECTION.
       01  WS-FS-DIVERG          PIC X(02).
           88 WS-FS-DIV-OK       VALUE '00'.
           88 WS-FS-DIV-FIM      VALUE '10'.
       01  WS-CONTADORES.
           05 WS-QTD-LIDOS        PIC 9(07) VALUE ZERO.
           05 WS-QTD-PS           PIC 9(07) VALUE ZERO.
           05 WS-QTD-FS           PIC 9(07) VALUE ZERO.
           05 WS-QTD-PM           PIC 9(07) VALUE ZERO.

       01  WS-TOTAIS.
           05 WS-VLR-FAT-PS       PIC 9(11)V99 VALUE ZERO.
           05 WS-VLR-FAT-FS       PIC 9(11)V99 VALUE ZERO.
           05 WS-VLR-FAT-PM       PIC 9(11)V99 VALUE ZERO.

           05 WS-VLR-PAG-PS       PIC 9(11)V99 VALUE ZERO.
           05 WS-VLR-PAG-FS       PIC 9(11)V99 VALUE ZERO.
           05 WS-VLR-PAG-PM       PIC 9(11)V99 VALUE ZERO.

       01  WS-TOT-FATURA          PIC 9(13)V99 VALUE ZERO.
       01  WS-TOT-PAGO            PIC 9(13)V99 VALUE ZERO.

       
       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR
              THRU 1000-FIM-INICIALIZAR

           PERFORM 2000-PROCESSAR
              THRU 2000-FIM-PROCESSAR
              UNTIL WS-FS-DIV-FIM

           PERFORM 3000-FINALIZAR
              THRU 3000-FIM-FINALIZAR

           GOBACK.
       1000-INICIALIZAR.

           OPEN INPUT ARQ-DIVERG

           IF NOT WS-FS-DIV-OK
              DISPLAY 'FATRESUM - ERRO OPEN FATURA.DIVERG'
              DISPLAY 'FS: ' WS-FS-DIVERG
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF

           PERFORM 8000-LER-DIVERGENCIA
              THRU 8000-FIM-LER-DIVERGENCIA
           .
       1000-FIM-INICIALIZAR.
           EXIT.
       2000-PROCESSAR.

           EVALUATE TRUE

           WHEN DV-PAGTO-SEM-FATURA
            ADD 1 TO WS-QTD-PS
            ADD DV-VALOR-FATURA TO WS-VLR-FAT-PS
            ADD DV-VALOR-PAGO   TO WS-VLR-PAG-PS

       WHEN DV-FATURA-SEM-PAGTO
            ADD 1 TO WS-QTD-FS
            ADD DV-VALOR-FATURA TO WS-VLR-FAT-FS
            ADD DV-VALOR-PAGO   TO WS-VLR-PAG-FS

       WHEN DV-PAGTO-A-MENOR
            ADD 1 TO WS-QTD-PM
            ADD DV-VALOR-FATURA TO WS-VLR-FAT-PM
            ADD DV-VALOR-PAGO   TO WS-VLR-PAG-PM

       WHEN OTHER
            DISPLAY 'FATRESUM - TIPO INVALIDO: ' DV-TIPO
            PERFORM 9900-ENCERRAR-COM-ERRO

       END-EVALUATE

       PERFORM 8000-LER-DIVERGENCIA
           THRU 8000-FIM-LER-DIVERGENCIA
       .

       2000-FIM-PROCESSAR.
       EXIT.

       3000-FINALIZAR.
           CLOSE ARQ-DIVERG

           IF NOT WS-FS-DIV-OK
              DISPLAY 'FATRESUM - ERRO CLOSE FATURA.DIVERG'
              DISPLAY 'FS: ' WS-FS-DIVERG
              MOVE 16 TO RETURN-CODE
           END-IF

           COMPUTE WS-TOT-FATURA =
                   WS-VLR-FAT-PS
                 + WS-VLR-FAT-FS
                 + WS-VLR-FAT-PM

           COMPUTE WS-TOT-PAGO =
                   WS-VLR-PAG-PS
                 + WS-VLR-PAG-FS
                 + WS-VLR-PAG-PM

           DISPLAY '*** RESUMO DE DIVERGENCIAS ***'

           DISPLAY 'PS - PAGTO SEM FATURA'
           DISPLAY 'QTD       : ' WS-QTD-PS
           DISPLAY 'VAL FAT   : ' WS-VLR-FAT-PS
           DISPLAY 'VAL PAGO  : ' WS-VLR-PAG-PS

           DISPLAY 'FS - FATURA SEM PAGTO'
           DISPLAY 'QTD       : ' WS-QTD-FS
           DISPLAY 'VAL FAT   : ' WS-VLR-FAT-FS
           DISPLAY 'VAL PAGO  : ' WS-VLR-PAG-FS

           DISPLAY 'PM - PAGTO A MENOR'
           DISPLAY 'QTD       : ' WS-QTD-PM
           DISPLAY 'VAL FAT   : ' WS-VLR-FAT-PM
           DISPLAY 'VAL PAGO  : ' WS-VLR-PAG-PM

           DISPLAY 'TOTAL LIDOS : ' WS-QTD-LIDOS
           DISPLAY 'TOTAL FATURA: ' WS-TOT-FATURA
           DISPLAY 'TOTAL PAGO  : ' WS-TOT-PAGO

           IF WS-QTD-LIDOS NOT =
              WS-QTD-PS +
              WS-QTD-FS +
              WS-QTD-PM

              DISPLAY 'FATRESUM - ERRO DE FECHAMENTO'
              MOVE 16 TO RETURN-CODE
           ELSE
              IF RETURN-CODE = ZERO
                 MOVE 0 TO RETURN-CODE
              END-IF
           END-IF
           .

       3000-FIM-FINALIZAR.
           EXIT.
       8000-LER-DIVERGENCIA.

           READ ARQ-DIVERG
              AT END
                 CONTINUE
           END-READ

           IF NOT WS-FS-DIV-OK
              AND NOT WS-FS-DIV-FIM

              DISPLAY 'FATRESUM - ERRO READ FATURA.DIVERG'
              DISPLAY 'FS: ' WS-FS-DIVERG

              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF

           IF WS-FS-DIV-OK
              ADD 1 TO WS-QTD-LIDOS
           END-IF
           .

       8000-FIM-LER-DIVERGENCIA.
           EXIT.
       9900-ENCERRAR-COM-ERRO.

           MOVE 16 TO RETURN-CODE
           STOP RUN
           .