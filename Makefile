.PHONY: format
format:
	git ls-files '*.hs' | xargs fourmolu --mode inplace
