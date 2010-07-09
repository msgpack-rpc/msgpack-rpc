
def generate(doc, outdir, langdir)
	ldir = "#{langdir}/java"

	require 'fileutils'
	nss = doc.namespace(:java)
	nspath = File.join(outdir, *nss)
	FileUtils.mkdir_p(nspath)

	doc.data[:common_mpl] = "#{ldir}/common.mpl"
	const = []

	doc.each do |d|
		case d
		when AST::Constant
			const << d
		when AST::Typedef
			#FIXME
			#Mplex.write("#{ldir}/typedef.mpl", "#{nspath}/#{d.name}.mpl", doc)
		when AST::Enum
			Mplex.write("#{ldir}/enum.mpl", "#{nspath}/#{d.name}.java", d)
		when AST::Struct, AST::Exception
			Mplex.write("#{ldir}/struct.mpl", "#{nspath}/#{d.name}.java", d)
		when AST::Service
			#FIXME
			#Mplex.write("#{ldir}/server.mpl", "#{nspath}/#{d.name}Server.java", doc)
			#Mplex.write("#{ldir}/client.mpl", "#{nspath}/#{d.name}Client.java", doc)
		end
	end

	unless const.empty?
		d = AST::Data.new(doc, const)
		Mplex.write("#{ldir}/const.mpl", "#{nspath}/Constants.java", d)
	end
end

