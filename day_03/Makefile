.PHONY: clean prepare execute
prepare: main
execute: main
	./main
clean:
	-rm ./*.hi ./*.o ./main
main:
	ghc *.hs -o ./main
