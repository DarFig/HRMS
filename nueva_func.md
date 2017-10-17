## Pasos para la nueva funcionalidad

### LIST BRANCH FILE

- añadida al menú inicial como opción 3
- al ser seleccionada llama a nuevo "módulo" EMPBRANCHLIST
- al finalizar la division del  EMPWRITE se pone el código correspondiente a la división EMPBRANCHLIST
- el modo de usar y acceder al branchfile será secuencial para ir leyendo todo el contenido(no por clave)
- este modo se ha cambiado en todos los accesos al fichero
- al cambiar a modo secuencial hay que abrir el archivo en escritura con la opción de EXTEND (OPEN EXTEND) para poder seguir escribiendo.
- cambiada segun el apartado 4.b
