
SUBDIRS = test

DOC_FILES = \
		README.md

CPP_DIST = \
		lang/cpp/header.mpl \
		lang/cpp/rpc.mpl \
		lang/cpp/source.mpl \
		lang/cpp/types.mpl \
		lang/cpp.rb

LANG_DIST = \
		$(CPP_DIST) \
		lang/lang.rb

RUBYLIB = \
		rubylib/treetop.rb \
		rubylib/treetop/README.md \
		rubylib/treetop/LICENSE \
		rubylib/treetop/runtime.rb \
		rubylib/treetop/runtime/compiled_parser.rb \
		rubylib/treetop/runtime/interval_skip_list.rb \
		rubylib/treetop/runtime/syntax_node.rb \
		rubylib/treetop/runtime/terminal_parse_failure.rb \
		rubylib/treetop/runtime/terminal_parse_failure_debug.rb \
		rubylib/treetop/runtime/terminal_syntax_node.rb \
		rubylib/treetop/runtime/interval_skip_list/head_node.rb \
		rubylib/treetop/runtime/interval_skip_list/interval_skip_list.rb \
		rubylib/treetop/runtime/interval_skip_list/node.rb \
		rubylib/rubygems

mprpcgen:
	echo '#!$(RUBY_PATH)' > $@
	echo '$$LOAD_PATH.unshift("$(pkgdatadir)/rubylib")' >> $@
	echo '$$LOAD_PATH.unshift("$(pkgdatadir)/lang")' >> $@
	echo 'load "$(pkgdatadir)/mprpcgen.rb"' >> $@
	chmod 755 $@

bin_SCRIPTS = mprpcgen

MOSTLYCLEANFILES = mprpcgen

mprpcgendir = $(pkgdatadir)

nobase_mprpcgen_DATA = \
		$(LANG_DIST) \
		$(RUBYLIB) \
		mprpcgen.rb

corelibdir = $(pkgdatadir)/rubylib

corelib_DATA = \
		mplex.rb \
		mprpc_ast.rb \
		mprpc_idl.rb

EXTRA_DIST = \
		$(LANG_DIST) \
		$(RUBYLIB) \
		$(DOC_FILES) \
		mprpcgen.rb \
		mplex.rb \
		mprpc_ast.rb \
		mprpc_idl.rb

