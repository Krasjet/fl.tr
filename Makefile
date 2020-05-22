.PHONY: test build ghci ghci-test ghcid

test:
	@stack test

build:
	@stack build

ghci:
	@stack ghci

ghci-test:
	@stack ghci --test

ghcid:
	@ghcid \
		--command "stack ghci --test" \
		--test "main"

doc:
	@stack haddock --open fl-tr
