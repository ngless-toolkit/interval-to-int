all:
	stack build
check: test
test:
	stack build --test
fast:
	stack build --fast
