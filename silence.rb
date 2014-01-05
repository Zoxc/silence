require 'rbconfig'
IsWindows = (RbConfig::CONFIG['host_os'] =~ /mswin|mingw/)

class CompileError < Exception
end

require_relative 'ast'
require_relative 'core'
require_relative 'gen'
require_relative 'gen-func'
require_relative 'print'
require_relative 'types'
require_relative 'infer'
require_relative 'type-context'
require_relative 'parser'

InferArgs = InferContext::InferArgs.new({}, [])
InferContext.infer_scope(Core::Program.scope, InferArgs)

def process(file, parent)
	puts "Processing #{file}"
	
	input = File.open(file) { |f| f.read }

	ast = Parser.new(input).program
	
	#puts print_ast(ast)
	begin
		ast.run_pass :declare_pass, false, (parent.scope if parent)
		ast.run_pass :sema, true
		ast.run_pass :ref_pass

		InferContext.infer_scope(ast.scope, InferArgs)
	rescue CompileError => error
		$stderr.puts "Fatal errors:", error.message
		$stderr.puts error.backtrace.join("\n")
		exit
	end
	
	File.open("output.cpp", "w") { |f| f.write Codegen.new.codegen(ast) }
	
	system 'g++ -std=gnu++0x -g -Wall -Wno-unused-value -Wno-self-assign -Wno-unused-variable output.cpp -o output'
	
	puts "Running..."
	system(IsWindows ? 'output.exe' : './output')
	
	ast
end

process(ARGV.first, Core::Program)