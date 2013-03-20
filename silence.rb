class CompileError < Exception
end

require_relative 'ast'
require_relative 'gen'
require_relative 'print'
require_relative 'types'
require_relative 'infer'
require_relative 'parser'

def process(ast, parent)
	ast.run_pass :declare_pass, false, parent.scope
	ast.run_pass :sema, true

	puts print_ast(ast)

	begin
		InferUtils.infer_scope(ast.scope)
	rescue CompileError => error
		$stderr.puts "Fatal errors:", error.message
		$stderr.puts error.backtrace.join("\n")
		exit
	end
end

input = File.open(ARGV.first) { |f| f.read }

ast = Parser.new(input).program

process(ast, AST::Builtin)

output = File.open("output.c", "w") { |f| f.write codegen(ast) }
`gcc output.c -Wall -o output`
`output.exe`