all: unit_tests_cnws.lpr test

unit_tests_cnws:
	lazbuild unit_tests_cnws.lpr
	
test: unit_tests_cnws
	./unit_tests_cnws -a

clean:
	rm -f unit_tests_cnws

.PHONY: clean test
