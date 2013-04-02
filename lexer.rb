require 'strscan'

class Lexer
	class Error < Exception
	end
	
	BraceIndex = {'(' => 0, '[' => 1, '{' => 2,
	              ')' => 0, ']' => 1, '}' => 2}
	
	class Block
		attr_reader :levels, :space
		
		def initialize(space)
			@space = space
			@levels = [0] * 3
		end
		
		def ignore?
			@levels.any? { |l| l > 0 }
		end
	end
	
	attr_reader :pos, :tok, :tok_str, :tok_val, :last_ended
	
	def initialize(str)
		@src = str
		@last_pos = 0
		@pos = 0
		@indent = ''
		@tok_str = ''
		@scanner = StringScanner.new(str)
		@blocks = []
		get_line_indent
		step
	end
	
	def indent
		[@indent, @indent_pos]
	end
	
	def block
		@blocks.last
	end
	
	def format_tok t, v
		v ? "#{v.inspect} (#{t})" : t.to_s
	end
	
	def source(pos = @pos, str = @tok_str)
		AST::Source.new(@src, pos...(pos + [1, tok_str.size].max))
	end
	
	def expected(v)
		raise Error, "Expected #{v}, but found #{format_tok(@tok, @tok_val)}\n#{source.format}"
	end
	
	def compare_indent(old, new)
		if old == new[0...old.size]
			if new.size > old.size
				:inc
			else
				:same
			end
		elsif new == old[0...new.size]
			:dec
		else
			:err
		end
	end
	
	def get_line_indent
		s = @scanner
		@indent_pos = s.pos
		@indent = s.scan(/[ \t]+/) || ''
		
		get_line_indent if s.scan(/\r\n|\n|\r/)
	end
	
	def indent_newline(baseline)
		raise "Newline required!" unless tok == :line
		get_line_indent
		
		r = false
		
		case compare_indent(baseline.first, @indent)
			when :inc
				@blocks << Block.new(baseline)
				r = true
				@tok = nil
				step
			when :err
				raise(Error, "Unable to find indentation size\n#{source(@indent_pos, @indent).format}\nRelative to baseline:\n#{source(baseline.last, baseline.first).format}")
			else
				handle_line
		end
		r
	end
	
	def handle_line
		@tok = nil
		
		if !block || block.ignore?
			step
			return
		end
		
		case compare_indent(block.space.first, @indent)
			when :dec, :same
				@blocks.pop
				i = 1
				while block && ([:dec, :same].include? compare_indent(block.space.first, @indent))
					i += 1
					@blocks.pop
				end
				@tok_str, @tok, @tok_val = [@indent, :deindent, i]
				produce_token
			when :inc
				step
			when :err
				raise(Error, "Mismatching indentation\n#{source(@indent_pos, @indent).format}\nRelative to baseline:\n#{source(block.space.last, block.space.first).format}")
		end
	end
	
	def pop_brace_level(brace)
		i = @blocks.size - 1
		
		@blocks.reverse.each_with_index do |block, i|
			lvl = block.levels[brace]
			if lvl > 0
				block.levels[brace] = lvl - 1
				@blocks = @blocks[0..(-1-i)]
				return i
			end
		end
		
		return 0
	end
	
	def find_token
		s = @scanner
		@pos = @scanner.pos
		case
			when v = s.scan(/[ \t]+/)
				find_token
			when v = s.scan(/\r\n|\n|\r/)
				[v, :line, true]
			when v = s.scan(/[0-9]+/)
				[v, :int, v.to_i]
			when v = s.scan(/[A-Za-z_]+\w*/)
				[v, :id, v.to_sym]
			when v = s.scan(/==|:=|::|->/)
				[v, :sym, v]
			when v = s.scan(/[(\[{]/)
				(block.levels[BraceIndex[v]] += 1) if block
				[v, :sym, v]
			when v = s.check(/[)\]}]/)
				if block
					p = pop_brace_level(BraceIndex[v])
					return ['', :deindent, p] if p > 0
				end
				s.scan(/[)\]}]/)
				[v, :sym, v]
			when v = s.scan(/[\.*+=\-,:]/)
				[v, :sym, v]
			when s.eos?
				@pos = @src.size - 1
				if block
					r = ['', :deindent, @blocks.size]
					@blocks = []
					r
				else
					['', :eos, true]
				end
			else
				puts "Unknown char '#{s.getch()}'"
				find_token
		end
	end
	
	def produce_token
		#puts "token: #{@tok}, #{@tok_val}\n#{source.format}"
	end

	def step_deindent
		raise 'Requires deindent' if tok != :deindent
		if @tok_val == 1
			@tok = nil
			step
		else
			@tok_val -= 1
			produce_token
		end
	end
	
	def step_line
		raise 'Requires line' if tok != :line
		@tok = nil
		get_line_indent
		handle_line
	end
	
	def step
		raise 'Stepping over line' if tok == :line
		raise 'Stepping over deindent' if tok == :deindent
		@last_ended = @pos + @tok_str.size
		raise if @token == :eos
		@tok_str, @tok, @tok_val = find_token
		produce_token
	end
end