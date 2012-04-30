EXEC    := mazes
OPTIONS := -thread
CMAS    := graphics.cma unix.cma threads.cma

main:
	ocamlc $(OPTIONS) $(CMAS) square.ml -o $(EXEC)

main:
	ocamlc $(OPTIONS) $(CMAS) bee.ml -o $(EXEC)

clean:
	rm *.cmi *.cmo $(EXEC)
