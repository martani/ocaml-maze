EXEC    := mazes
OPTIONS := -thread
CMAS    := graphics.cma unix.cma threads.cma

main:
	ocamlc $(OPTIONS) $(CMAS) square.ml -o $(EXEC)

<<<<<<< HEAD
main:
=======
bee:
>>>>>>> origin/master
	ocamlc $(OPTIONS) $(CMAS) bee.ml -o $(EXEC)

clean:
	rm *.cmi *.cmo $(EXEC)
