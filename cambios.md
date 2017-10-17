

- DISPLAY (x y) ERASE -> DISPLAY " " WITH BLANK SCREEN.
- DISPLAY (x y) "..." -> DISPLAY "..." LINE x COL y.
- ACCEPT (x y) CHOICE. -> ACCEPT CHOICE LINE x COL y.

- lineas originalmente 390 y 391 escribían en la misma linea del display

añadir al final:	
- END PROGRAM EMPWRITE.
- END PROGRAM EMPREAD.
- END PROGRAM MAINHRMS.

- <DIVISION EMP> renombrada a <EMPWRITE>.


- Después de <"PRESS ENTER TO RETURN TO HRMS READ MENU" LINE 20 COL 10.>
añadir <ACCEPT ENTER LINE 20 COL 50.> para que espera al ENTER para
ello añadir en el <WORKING-STORAGE SECTION.> de <EMPREAD.> <77 ENTER PIC X.>

- cambiados los formatos de algunos textos de salida de display, y códigos numéricos al intentar abrir archivos.




