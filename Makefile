SUFFIX = 

ifeq ($(OS),Windows_NT)
	SUFFIX = .exe
endif

.PHONY: all build test clean

all: build

build:
	./_tools/go-inline *.go && go fmt . && go build cmd/glua/glua.go

test:
	./_tools/go-inline *.go && go fmt . &&  go test

clean:
	rm -f glua${SUFFIX}