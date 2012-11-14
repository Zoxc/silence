class CompileError < Exception
end

require_relative 'ast'
require_relative 'gen'
require_relative 'print'
require_relative 'types'
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

ast.run_pass :declare_pass, false, AST::BuiltinScope
ast.run_pass :sema, true

#puts print_ast(ast)

begin
	infer_scope ast
rescue CompileError => error
	$stderr.puts "Fatal errors:", error.message
	$stderr.puts error.backtrace.join("\n")
	exit
end

output = File.open("output.c", "w") { |f| f.write codegen(ast) }
`gcc output.c -Wall -o output`
`output.exe`