INPUT_FILE := input.txt
.PHONY: clean prepare execute
prepare: main
execute: main
	./main $(INPUT_FILE)
clean:
	-rm ./main
main:
	go build -o main ./main.go
