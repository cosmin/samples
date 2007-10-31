require 'find'
location = ARGV[0]
regexp = Regexp.compile(ARGV[1])

print "Location = #{location}\n"
print "Regexp = #{ARGV[1]}\n"


Find.find(location) do |path|
    if FileTest.file?(path)
      name =  path.gsub(/^.*(\\|\/)/, '') + "::" + FileTest.size(path).to_s
      if name =~ regexp
        print "Deleting #{path}\n"
        File.delete(path)
      end
    end
end
