RUBY=ruby
TTVER=treetop-1.4.8

all: parser.rb

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

parser.tt: parser.tt.mpl mplex.rb
	$(RUBY) -rmplex -e 'Mplex.write("parser.tt.mpl", "$@")'

parser.rb: $(TTVER)/bin/tt parser.tt rubylib/rubygems
	$(RUBY) -I$(TTVER)/lib $(TTVER)/bin/tt -o $@ parser.tt

clean:
	rm -rf $(TTVER) rubylib parser.tt parser.rb

distclean: clean
	rm -rf $(TTVER).gem

.PHONY: all clean distclean

