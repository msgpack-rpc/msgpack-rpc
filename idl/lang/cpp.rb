
def generate(doc, outdir, langdir)
	doc.data[:common_mpl] = "#{langdir}/cpp/common.mpl"

	Mplex.write("#{langdir}/cpp/types.mpl", "#{outdir}/types.hpp", doc)

	doc.services.each do |s|
		Mplex.write("#{langdir}/cpp/header.mpl", "#{outdir}/#{s.name}.hpp", s)
		Mplex.write("#{langdir}/cpp/source.mpl", "#{outdir}/#{s.name}.cpp", s)
	end

	Mplex.write("#{langdir}/cpp/rpc.mpl", "#{outdir}/rpc.hpp", doc)
end

