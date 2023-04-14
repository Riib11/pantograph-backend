build:
	spago build

run: build
	spago run

test: build
	spago test

clean:
	rm -rf output
