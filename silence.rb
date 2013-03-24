class CompileError < Exception
end

require_relative 'ast'
require_relative 'gen'
require_relative 'print'
require_relative 'types'
require_relative 'infer'
require_relative 'parser'

InferUtils.infer_scope(AST::Builtin.scope)

def process(file, parent)
	puts "Processing #{file}"
	
	input = File.open(file) { |f| f.read }

	ast = Parser.new(input).program

	ast.run_pass :declare_pass, false, (parent.scope if parent)
	ast.run_pass :sema, true

	puts print_ast(ast)

	begin
		InferUtils.infer_scope(ast.scope)
	rescue CompileError => error
		$stderr.puts "Fatal errors:", error.message
		$stderr.puts error.backtrace.join("\n")
		exit
	end
	
	ast
end

process(ARGV.first, AST::Builtin)

output = File.open("output.c", "w") { |f| f.write codegen(ast) }
`gcc output.c -Wall -o output`
`output.exe`