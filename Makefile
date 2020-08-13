all:
	stack build
check: test
test:
	stack build --test --fast
fast:
	stack build --fast
