.PHONY: book
book:
	cd book && mdbook build


print-%: ; @echo $*=$($*)
