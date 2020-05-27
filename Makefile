.PHONY: test build ghci ghci-test ghcid

test: codegen
	@stack test

build: codegen
	@stack build

ghci: codegen
	@stack ghci

ghci-test: codegen
	@stack ghci --test

ghcid: codegen
	@ghcid \
		--command "stack ghci --test" \
		--test "main"

doc: codegen
	@stack haddock --open fl-tr

codegen:
	$(MAKE) -C ./src/Text/Pandoc/Fltr/Pygments/

clean:
	@stack clean
	$(MAKE) -C ./src/Text/Pandoc/Fltr/Pygments/ clean
