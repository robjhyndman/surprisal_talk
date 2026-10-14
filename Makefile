.PHONY: all update

all:
	Rscript -e 'targets::tar_make()'

# Remove the installed weird so uvr reinstalls the latest GitHub version
update:
	rm -rf .uvr/library/weird
	uvr update
