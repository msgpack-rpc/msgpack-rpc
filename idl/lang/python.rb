
def generate(doc, outdir, langdir)
    ldir = "#{langdir}/python"

    doc.data[:common_mpl] = "#{ldir}/common.mpl"

    namespace = doc.namespace(:python)
    if not namespace.size > 0
        namespace = 'msgpackidl'
    end
    packdir = outdir + "/" + namespace.to_s.tr('.', '/')
    begin
        Dir::mkdir(packdir)
    rescue
    end
    File.open("#{packdir}/__init__.py", "w"){}
    Mplex.write("#{ldir}/types.mpl", "#{packdir}/idltypes.py", doc)

    doc.services.each do |s|
        obase = "#{packdir}/#{s.name}"
        if doc.conf[:devel]
            puts "Found service #{s.name}"
        else
            begin
                Dir::mkdir(obase)
            rescue
            end

            Mplex.write("#{ldir}/service.mpl", "#{obase}/__init__.py", s)
            Mplex.write("#{ldir}/service_client.mpl", "#{obase}/client.py", s)
            Mplex.write("#{ldir}/service_server.mpl", "#{obase}/server.py", s)
        end
    end

    if not doc.conf[:devel]  # TODO
        doc.data[:pack] = namespace.to_s
        doc.data[:mode] = :client
        Mplex.write("#{ldir}/requires.mpl", "#{outdir}/client.py", doc)

        doc.data[:mode] = :server
        Mplex.write("#{ldir}/requires.mpl", "#{outdir}/server.py", doc)
    end
end

