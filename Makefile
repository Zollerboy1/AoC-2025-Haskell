include .env

DAY ?= $(shell date +"%d" | bc)

ifdef AOC_YEAR
YEAR := $(AOC_YEAR)
else
YEAR := 2025
endif

getinput:
	@curl -Ssl "https://adventofcode.com/$(YEAR)/day/$(DAY)/input" \
	  -A "getinput by Zollerboy1" \
	  -H "cookie: session=$(AOC_SESSION)" > day$(DAY).txt
