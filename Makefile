EXECS   := square bee
OPTIONS := -thread
CMAS    := graphics.cma unix.cma threads.cma

all:$(EXECS)

square:square.ml
	ocamlc $(OPTIONS) $(CMAS) square.ml -o square

bee:bee.ml
	ocamlc $(OPTIONS) $(CMAS) bee.ml -o bee

clean:
	rm -f *.cmi *.cmo $(EXECS)
