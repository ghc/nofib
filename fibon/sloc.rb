#!/usr/bin/env ruby

# Simple script to count and display lines of code in a benchmark
def wcTotal(dir)
  cmd = %@find "#{dir}" \\( \\( -name "*.hs" \\) -or \\( -name "*.lhs" \\) \\) \\! -name "Instance.hs" | xargs wc -l @
  #puts cmd
  s = `#{cmd}`
  if s.split("\n").length == 0 then
    tot = "0"
  elsif s.split("\n").length == 1 then
    tot = s
  else
    tot = s.split("\n").reject{|l| not (l =~ /\s*\d+\s+total/)}.first
  end
  tot.match(/\s*(\d+)\s+/)
  $1
end

def ppWc(n, h=nil)
  h ||= n
  printf("  %-20s%10d\n", h, (wcTotal n).to_i)
end

if __FILE__ == $0 then
  dirs = ARGV
  if dirs == [] then dirs = ["*"] end
  dirs.each do |d|
    Dir[d].each do |group|
      if File.directory?(group) then
        puts group 
        Dir[group+"/*"].each do |bm|
          if File.directory?(bm) then
            ppWc(bm, File.basename(bm))
          end
        end
        puts "  " + "-" * 30
        ppWc(group, "TOTAL")
        puts
      end
    end
  end
end

