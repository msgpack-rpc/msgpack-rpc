
SUBDIRS = test

DOC_FILES = \
		README.md

CPP_DIST = \
		lang/cpp/common.mpl \
		lang/cpp/headers.mpl \
		lang/cpp/service.mpl \
		lang/cpp/service_client.mpl \
		lang/cpp/service_server.mpl \
		lang/cpp/service_source.mpl \
		lang/cpp/types.mpl \
		lang/cpp.rb

RUBY_DIST = \
		lang/ruby/common.mpl \
		lang/ruby/requires.mpl \
		lang/ruby/service.mpl \
		lang/ruby/service_client.mpl \
		lang/ruby/service_server.mpl \
		lang/ruby/types.mpl \
		lang/ruby.rb

JAVA_DIST = \
		lang/java/common.mpl \
		lang/java/const.mpl \
		lang/java/enum.mpl \
		lang/java/struct.mpl \
		lang/java.rb

LANG_DIST = \
		$(CPP_DIST) \
		$(RUBY_DIST) \
		$(JAVA_DIST)

msgpack-idl:
	echo '#!$(RUBY_PATH)' > $@
	echo '$$LOAD_PATH.unshift("$(pkgdatadir)/rubylib")' >> $@
	echo 'load "$(pkgdatadir)/command.rb"' >> $@
	chmod 755 $@

bin_SCRIPTS = msgpack-idl

MOSTLYCLEANFILES = msgpack-idl

msgpack_idldir = $(pkgdatadir)

nobase_msgpack_idl_DATA = \
		$(LANG_DIST) \
		command.rb

corelibdir = $(pkgdatadir)/rubylib

corelib_DATA = \
		mplex.rb \
		ast.rb \
		parser.rb

EXTRA_DIST = \
		$(LANG_DIST) \
		$(DOC_FILES) \
		$(corelib_DATA) \
		command.rb

