require_relative 'ast'
require_relative 'gen'

Treetop.load "grammar"

parser = HushParser.new
parser.root = :program

input = File.open(ARGV.first) { |f| f.read }

result = parser.parse(input)

unless result
	puts "failed - #{parser.failure_reason.inspect}"
    puts input.lines.to_a[parser.failure_line - 1]
    puts "#{'~' * (parser.failure_column - 1)}^"
	exit
end

ast = result.ast
puts ast.inspect

puts codegen(ast)