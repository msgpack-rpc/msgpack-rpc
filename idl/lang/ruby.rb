
def generate(doc, outdir, langdir)
	doc.data[:common_mpl] = "#{langdir}/ruby/common.mpl"
	Mplex.write("#{langdir}/ruby/types.mpl", "#{outdir}/types.rb", doc)
	doc.services.each do |s|
		Mplex.write("#{langdir}/ruby/service.mpl", "#{outdir}/#{s.type_name}.rb", s)
	end
end

