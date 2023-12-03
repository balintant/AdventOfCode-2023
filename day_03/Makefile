INPUT_FILE := input.txt
COMMAND_FLAGS := -v2
.PHONY: clean prepare execute
prepare: main
execute: main
	./main $(COMMAND_FLAGS) $(INPUT_FILE)
clean:
	-rm ./*.hi ./*.o ./main
main:
	ghc *.hs -o ./main
