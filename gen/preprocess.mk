RUBY=ruby
TTVER=treetop-1.4.5

all: mprpc_idl.rb

TTURL = http://rubygems.org/downloads/$(TTVER).gem

$(TTVER).gem:
	wget $(TTURL) || curl -O $(TTURL) || fetch $(TTURL)

treetop/bin/tt: $(TTVER).gem
	mkdir -p treetop
	cd treetop && tar xvf ../$(TTVER).gem
	cd treetop && tar zxvf data.tar.gz
	touch $@

rubylib/rubygems: treetop/bin/tt
	mkdir -p rubylib
	cp -rf treetop/lib/* rubylib/
	cp -rf treetop/README.md rubylib/treetop/
	cp -rf treetop/LICENSE rubylib/treetop/
	echo "# dummy" > $@

mprpc_idl.tt: mprpc_idl.tt.mpl mplex
	$(RUBY) mplex mprpc_idl.tt.mpl -o $@

mprpc_idl.rb: treetop/bin/tt mprpc_idl.tt rubylib/rubygems
	$(RUBY) -Irubylib treetop/bin/tt -o $@ mprpc_idl.tt

clean:
	rm -rf treetop rubylib mprpc_idl.tt mprpc_idl.rb

distclean: clean
	rm -rf $(TTVER).gem

.PHONY: all clean distclean

