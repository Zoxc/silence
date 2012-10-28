require 'treetop'

Treetop.load "grammar"

parser = HushParser.new
parser.root = :program

input = File.open(ARGV.first) { |f| f.read }

result = parser.parse(input)

if result
	puts "ok - #{result}"
else
	puts "failed - #{parser.failure_reason.inspect}"
    puts input.lines.to_a[parser.failure_line - 1]
    puts "#{'~' * (parser.failure_column - 1)}^"
end