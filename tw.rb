#!/bin/env ruby

require 'yaml'

def parse_body(body, &blk)
  @body_sep = "%"
  lines = []
  body.split(@body_sep).each do |b|
    hash = YAML::load(b)
    next unless hash
    if block_given? then
      yield hash
    else
      lines << b
    end
  end
  lines
end


if __FILE__ == $0 then
  #srand(99)
  File.open("log.txt", "w") do |log|
  parse_body(ARGF.read) do |specifics|
    specifics.each do |k,v|
      opcnt = rand 1000
      puts "#{k}|#{opcnt}|#{v}"
      log.puts "#{k}|#{opcnt}|#{v}"
    end
    puts "%"
  end
  end
end
