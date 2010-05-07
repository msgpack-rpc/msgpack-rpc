mprpcgen: parse.rb run.rb ast.rb parse.rb lex.rb mplex.rb
	echo '#!/usr/bin/env ruby' > $@.tmp
	grep -v "^require" run.rb | cat ast.rb parse.rb lex.rb mplex.rb - >> $@.tmp
	mv $@.tmp $@
	chmod 755 $@

parse.rb: parse.y
	racc parse.y -o parse.rb

clean:
	rm -f mprpcgen parse.rb

