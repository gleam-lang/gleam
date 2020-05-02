.PHONY: help
help:
	@cat $(MAKEFILE_LIST) | grep -E '^[a-zA-Z_-]+:.*?## .*$$' | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: test ## Run all tests
test:
	cargo test

.PHONY: test-watch
test-watch: ## Run tests when files change
	watchexec -e rs,lalrpop,html "echo; cargo test; echo; echo"
