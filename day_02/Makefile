INPUT_FILE := input.txt
TARGET_CUBES := 14 blue, 13 green, 12 red
.PHONY: clean prepare execute
prepare: main
execute: main
	./main $(INPUT_FILE) $(TARGET_CUBES)
clean:
	-rm ./main
main:
	swiftc *.swift -o ./main
