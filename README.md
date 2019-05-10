# numera
Un sencillo programa para añadir números a ficheros

Con `numera` se resuelve el problema de ir creando versiones de un fichero. Lo que hace es añadir
un número delante de la extensión. Por ejemplo `f.c` pasa a `f.1.c`. Si ya hay ficheros con números,
añade el siguiente al mayor de ellos. Si el fichero ya tiene un número antes de la extensión, no se
hace nada a menos que se use la opción `-t` o `--todo`. 
