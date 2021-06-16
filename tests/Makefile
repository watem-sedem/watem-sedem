all: unit_tests_cnws.lpr test

unit_tests_cnws:
	lazbuild unit_tests_cnws.lpr
	
test: unit_tests_cnws
	./unit_tests_cnws -a --format=plain | tee unit_test_result
	! grep Error unit_test_result

clean:
	rm -f unit_tests_cnws

.PHONY: clean test
