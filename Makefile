.PHONY: all update clean images

all:
	Rscript -e 'targets::tar_make()'

# Remove the installed weird so uvr reinstalls the latest GitHub version
update:
	rm -rf .uvr/library/weird
	uvr update

# Mark every target out of date, so the next `make` rebuilds everything (e.g.
# after a package update; targets doesn't track package versions). Unlike
# tar_destroy(), the stored results stay readable until they are rebuilt.
clean:
	Rscript -e 'targets::tar_invalidate(everything())'

# Optimise the hand-made images losslessly before committing (instead of ImgBot):
# oxipng for PNGs, svgo for SVGs. Tools missing from PATH are skipped.
OXIPNG := $(shell command -v oxipng || ls ~/.cargo/bin/oxipng 2>/dev/null)
SVGO := $(shell command -v svgo)
images:
	$(if $(OXIPNG),$(OXIPNG) --strip safe images/*.png,@echo "oxipng not found: PNGs skipped")
	$(if $(SVGO),$(SVGO) --quiet images/*.svg,@echo "svgo not found: SVGs skipped")
