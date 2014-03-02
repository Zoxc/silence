require 'pathname'
require 'rbconfig'
require 'benchmark'
IsWindows = (RbConfig::CONFIG['host_os'] =~ /mswin|mingw/)

class CompileError < StandardError
end

class Silence
	def self.set_program(program)
		@program = program
	end

	def self.get_program
		@program
	end

	Args = ARGV.dup
	Debug = Args.first == '-d'
	Args.shift if Debug
	Bare = Args.first == '-b'
	Args.shift if Bare
	def self.puts(*args)
		$stdout.puts(*args) if Debug
	end
end

require_relative 'ast'
require_relative 'gen'
require_relative 'gen-func'
require_relative 'print'
require_relative 'types'
require_relative 'infer'
require_relative 'type-context'
require_relative 'parser'

def fpath(f)
	f = Pathname.new(f)
	if IsWindows && f.absolute? && f.to_s[0].downcase != Dir.pwd[0].downcase
		f.to_s
	else
		f.relative_path_from(Pathname.new(Dir.pwd)).to_s
	end
end

def parse(files, asts, file)
	return if files[file]
	files[file] = true

	Silence.puts "Parsing #{fpath file}"

	input = File.open(file) { |f| f.read }
	parser = Parser.new(AST::Input.new(input, fpath(file)))
	ast = parser.program

	asts.concat ast

	parser.imports.map do |i|
		i = File.absolute_path(i, File.dirname(file))
		i += ".hsh" if File.extname(i) == ""
		raise CompileError, "Unable to find file '#{fpath i}'\nImported in #{fpath file}" unless File.exist?(i)
		parse(files, asts, File.realpath(i))
	end
end

def process(file)
	files = {}
	asts = []

	time = Benchmark.measure do
		infer_args = InferContext::InferArgs.new({}, [])


		begin
			parse(files, asts, File.realpath(File.expand_path('../src/core.hsh', __FILE__))) unless Silence::Bare
			parse(files, asts, file)
			
			ast = AST::Program.new(AST::GlobalScope.new(asts))

			#puts print_ast(ast)
			ast.run_pass :declare_pass, false

			Silence.set_program(ast)
			require_relative 'core' # Core requires builtin things to be defined
			asts.concat Core::Nodes

			ast.run_pass :sema, true
			ast.run_pass :ref_pass

			InferContext.infer_core(infer_args)
			InferContext.infer_scope(ast.scope, infer_args)
			InferContext.infer_scope(Core::Generated, infer_args)

		rescue CompileError => error
			$stderr.puts "Failed to compile file:", error.message
			$stderr.puts error.backtrace.join("\n") if Silence::Debug
			exit
		end
		
		puts "Generating code..."
		File.open("output.cpp", "w") { |f| f.write Codegen.new.codegen(ast) }
	end
	
	puts "Compiled in #{time}"

	puts "Compiling output..."
	system "g++ -std=gnu++0x -g -Wall -Wno-unused-label -Wno-unused-variable -Wno-unused-value -Wno-self-assign -Wno-unused-variable #{File.expand_path('../hush.cpp', __FILE__)} output.cpp -o output"
	if $?.exitstatus != 0
		$stderr.puts "Failed to compile C++ output" 
		exit
	end
	puts "Running..."
	system(IsWindows ? 'output.exe' : './output')
	
	ast
end

process(File.realpath(Silence::Args.first))