BUNDLE=bundle
VENDOR_BUNDLE_DIR=vendor/bundle

all: parser.rb

$(VENDOR_BUNDLE_DIR): Gemfile
	$(BUNDLE) install --path $(VENDOR_BUNDLE_DIR)

parser.tt: parser.tt.mpl mplex.rb $(VENDOR_BUNDLE_DIR)
	$(BUNDLE) exec mplex -o $@ $<

parser.rb: parser.tt $(VENDOR_BUNDLE_DIR)
	$(BUNDLE) exec tt -o $@ parser.tt

clean:
	rm -rf parser.tt parser.rb

distclean: clean
	rm -rf $(VENDOR_BUNDLE_DIR)

.PHONY: all clean distclean

