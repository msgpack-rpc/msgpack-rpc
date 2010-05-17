RUBY=ruby
TTVER=treetop-1.4.5

all: mprpc_idl.rb

TTURL = http://rubygems.org/downloads/$(TTVER).gem

$(TTVER).gem:
	wget $(TTURL) || curl -O $(TTURL) || fetch $(TTURL)

$(TTVER)/bin/tt: $(TTVER).gem
	mkdir -p $(TTVER)
	cd $(TTVER) && tar xvf ../$(TTVER).gem
	cd $(TTVER) && tar zxvf data.tar.gz
	touch $@

rubylib/rubygems: $(TTVER)/bin/tt
	mkdir -p rubylib
	cp -rf $(TTVER)/lib/* rubylib/
	cp -rf $(TTVER)/README.md rubylib/treetop/
	cp -rf $(TTVER)/LICENSE rubylib/treetop/
	echo "# dummy" > $@

mprpc_idl.tt: mprpc_idl.tt.mpl mplex.rb
	$(RUBY) -rmplex -e 'Mplex.write("mprpc_idl.tt.mpl", "$@")'

mprpc_idl.rb: $(TTVER)/bin/tt mprpc_idl.tt rubylib/rubygems
	$(RUBY) -I$(TTVER)/lib $(TTVER)/bin/tt -o $@ mprpc_idl.tt

clean:
	rm -rf $(TTVER) rubylib mprpc_idl.tt mprpc_idl.rb

distclean: clean
	rm -rf $(TTVER).gem

.PHONY: all clean distclean

