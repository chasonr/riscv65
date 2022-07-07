#!/usr/bin/env ruby

simple_map = []
multi_map = []

File.foreach("CaseFolding.txt") do |line|
    # Strip comment
    sharp = line.index("#")
    line = line[0...sharp] if sharp

    # Strip leading and trailing blanks
    line.strip!
    # Skip if blank line
    next if line.empty?

    # Split into components
    rec = line.split(';').map {|s| s.strip}

    # Use the one-to-many case folding
    if rec[1] == "C" then
        simple_map.push([ rec[0].to_i(16), rec[2].to_i(16) ])
    elsif rec[1] == "F" then
        str = (rec[2].split(' ').map {|s| ch = s.to_i(16); ch < 0x10000 ? "\\u%04X" % ch : "\\U%08X" % ch}).join("")
        multi_map.push([ rec[0].to_i(16), str ])
    end
end

# Sort both lists by character
simple_map.sort! {|a, b| a[0] <=> b[0]}
multi_map.sort! {|a, b| a[0] <=> b[0]}

puts "// Case folding table generated from CaseFolding.txt"
puts "// Edits to this file will be discarded"
puts

# Print the simple map
puts "static const std::vector<UNI_simple_map> simple_map = {"
simple_map.each do |rec|
    puts "    { 0x%04X, 0x%04X }," % rec
end
puts "};"
puts
# Print the multi map
puts "static const std::vector<UNI_multi_map> multi_map = {"
multi_map.each do |rec|
    puts '    { 0x%04X, u"%s" },' % rec
end
puts "};"
