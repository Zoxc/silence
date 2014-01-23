require 'pathname'
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

def fpath(f)
	Pathname.new(f).relative_path_from(Pathname.new(Dir.pwd)).to_s
end

def parse(files, file)
	return if files[file]

	puts "Parsing #{fpath file}"

	input = File.open(file) { |f| f.read }
	parser = Parser.new(AST::Input.new(input, fpath(file)))
	ast = parser.program

	[ast] + parser.imports.map do |i|
		i = File.absolute_path(i, File.dirname(file))
		i += ".hs" if File.extname(i) == ""
		raise "Unable to find file '#{fpath i}'\nImported in #{fpath file}" unless File.exist?(i)
		parse(files, File.realpath(i))
	end
end

def process(file, parent)
	files = {}
	asts = parse(files, file).flatten

	ast = AST::Program.new(AST::GlobalScope.new(asts))

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
process(File.realpath(ARGV.first), Core::Program)