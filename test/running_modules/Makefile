.phony: build
build: clean erlang javascript

.phony: clean
clean:
	@rm -rf target-*

.phony: test
test:
	@echo test/running_modules
	@chmod +x run_tests
	@./run_tests
