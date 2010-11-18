#!/usr/bin/env ruby

added   = 0
removed = 0
IO.popen("git log --reverse --date=short --format=format:\"%ad %s\"") do |p|
  while not p.eof?
    line = p.gets 
    if line =~ /Added.*benchmark/ then
      puts line
      added += 1
    elsif line =~ /Deleted.*benchmark|Removed.*benchmark/
    then
      puts line
      removed += 1
    end
  end
end

puts ("-"*68)
puts "Added: #{added} Removed: #{removed} Total: #{added - removed}"
