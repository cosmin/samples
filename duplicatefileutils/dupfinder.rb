require 'find'
source = ARGV[0]
destination = ARGV[1]

print "Source = #{source}\n"
print "Destination = #{destination}\n"

files = {}
match_keys = []

to_examine = [source, destination]

to_examine.each do |examine|
  Find.find(examine) do |path|
    if FileTest.file?(path)
      print "=> #{path}\n"
      name =  path.gsub(/^.*(\\|\/)/, '') + "::" + FileTest.size(path).to_s
      #file_info =  FileInfo.new(FileTest.size(path), name)
      if files.has_key?(name)
        #print "=MATCH="
        files[name] << path
        match_keys << name
      elsif
        files[name] = [path]
      end
    end
  end
end


print "\n\n"
print "Listing matches...\n"
match_keys.each do |match|
  #file_name = match.name
  name = match
  paths = files[match]
  print " Verifying matches fo #{name}..."
  t_matches = {}
  t_match_keys = []
  paths.each do |path|
    checksum = 0
    File.open(path, "rb") do |file|
      file.each_byte {|x| checksum ^= x }
    end
    if t_matches.has_key? checksum
      t_matches[checksum] << path
      t_match_keys << checksum
    elsif
      t_matches[checksum] = [path]
    end
  end
  if t_match_keys.size > 0
    t_match_keys.each do |t_match|
      print " Matches for #{name} with checksum #{t_match}"
      t_paths = t_matches[t_match]
      t_paths.each do |t_path|
        print " -#{t_path}\n"   
      end
    end
    print "\n"   
  end
    
end
