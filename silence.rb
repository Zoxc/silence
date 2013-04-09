class CompileError < Exception
end

require_relative 'ast'
require_relative 'gen'
require_relative 'gen-func'
require_relative 'print'
require_relative 'types'
require_relative 'infer'
require_relative 'typeclass'
require_relative 'parser'

InferArgs = TypeContext::InferArgs.new({})
TypeContext.infer_scope(AST::Builtin.scope, InferArgs)

def process(file, parent)
	puts "Processing #{file}"
	
	input = File.open(file) { |f| f.read }

	ast = Parser.new(input).program
	
	#puts print_ast(ast)
	begin
		ast.run_pass :declare_pass, false, (parent.scope if parent)
		ast.run_pass :sema, true
		ast.run_pass :ref_pass

		TypeContext.infer_scope(ast.scope, InferArgs)
	rescue CompileError => error
		$stderr.puts "Fatal errors:", error.message
		$stderr.puts error.backtrace.join("\n")
		exit
	end
	
	output = File.open("output.cpp", "w") { |f| f.write Codegen.new(InferArgs).codegen(ast) }
	`g++ -std=gnu++0x -Wall -Wno-unused-value -Wno-unused-variable output.cpp -o output`
	`output.exe`
	
	ast
end

process(ARGV.first, AST::Builtin)