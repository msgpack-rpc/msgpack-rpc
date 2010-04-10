SUBDIRS = src test

MPLEX = $(RUBY) $(PWD)/mplex -r$(PWD)/mpl.rb
export MPLEX

prep: mpl.rb
	cd src && $(MAKE) prep

prepc:
	cd src && $(MAKE) prepc

