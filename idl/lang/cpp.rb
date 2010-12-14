
def generate(doc, outdir, langdir)
	ldir = "#{langdir}/cpp"

	doc.data[:common_mpl] = "#{ldir}/common.mpl"

	Mplex.write("#{ldir}/types.mpl", "#{outdir}/types.hpp", doc)
	doc.services.each do |s|
		obase = "#{outdir}/#{s.name}"
		Mplex.write("#{ldir}/service.mpl", "#{obase}.hpp", s)
		Mplex.write("#{ldir}/service_client.mpl", "#{obase}_client.hpp", s)
		Mplex.write("#{ldir}/service_server.mpl", "#{obase}_server.hpp", s)
		Mplex.write("#{ldir}/service_source.mpl", "#{obase}.cpp", s)
	end

	doc.data[:mode] = :client
	Mplex.write("#{ldir}/headers.mpl", "#{outdir}/client.hpp", doc)

	doc.data[:mode] = :server
	Mplex.write("#{ldir}/headers.mpl", "#{outdir}/server.hpp", doc)
end

