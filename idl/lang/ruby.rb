
def generate(doc, outdir, langdir)
	ldir = "#{langdir}/ruby"

	doc.data[:common_mpl] = "#{ldir}/common.mpl"

	Mplex.write("#{ldir}/types.mpl", "#{outdir}/types.rb", doc)
	doc.services.each do |s|
		obase = "#{outdir}/#{s.name}"
		Mplex.write("#{ldir}/service.mpl", "#{obase}.rb", s)
		Mplex.write("#{ldir}/service_client.mpl", "#{obase}_client.rb", s)
		Mplex.write("#{ldir}/service_server.mpl", "#{obase}_server.rb", s)
	end

	doc.data[:mode] = :client
	Mplex.write("#{ldir}/requires.mpl", "#{outdir}/client.rb", doc)


	doc.data[:mode] = :server
	Mplex.write("#{ldir}/requires.mpl", "#{outdir}/server.rb", doc)
end

