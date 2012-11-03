require_relative 'ast'
require_relative 'gen'
require_relative 'print'
require_relative 'infer'

Treetop.load "grammar"

parser = ASTParser.new
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

ast.run_pass :declare_pass
ast.run_pass :sema, true

puts print_ast(ast)

infer_scope ast

output = File.open("output.c", "w") { |f| f.write codegen(ast) }
`gcc output.c -Wall -o output`
`output.exe`