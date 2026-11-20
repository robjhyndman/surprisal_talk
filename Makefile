.PHONY: all update clean

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
