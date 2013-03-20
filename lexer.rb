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
	
	attr_reader :pos, :tok, :tok_str, :tok_val, :last_ended, :indent
	
	def initialize(str)
		@src = str
		@last_pos = 0
		@pos = 0
		@indent = ''
		@tok_str = ''
		@scanner = StringScanner.new(str)
		@blocks = []
		step
	end
	
	def block
		@blocks.last
	end
	
	def format_tok t, v
		v ? "#{v.inspect} (#{t})" : t.to_s
	end
	
	def source(pos = @pos, str = @tok_str)
		AST::Source.new(@src, pos...(pos + tok_str.size))
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
		
		if compare_indent(baseline, @indent) == :inc
			@blocks << Block.new(baseline)
			r = true
		else
			handle_line
		end
		step
		r
	end
	
	def handle_line
		@tok = nil
		
		unless block
			step
			return
		end
		
		case compare_indent(block.space, @indent)
			when :dec, :same
				@blocks.pop
				i = 1
				while [:dec, :same].include? compare_indent(block.space, @indent)
					i += 1
					@blocks.pop
				end
				@tok_str, @tok, @tok_val = [@indent, :deindent, i]
			when :inc
				step
			when :err
				raise(Error, "Mismatching indentation\n#{source(@indent_pos, @indent).format}")
		end
	end
	
	def pop_brace_level(brace)
		i = @blocks.size - 1
		
		while i > 0
			lvl = @blocks[i].levels[brace]
			if lvl > 0
				@blocks[i].levels[brace] = lvl - 1
				@blocks = @blocks[0..i]
				return (@blocks.size - 1) - i
			end
			i -= 1
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
				[v, :line]
			when v = s.scan(/[0-9]+/)
				[v, :int, v.to_i]
			when v = s.scan(/[A-Za-z_]+\w*/)
				[v, :id, v.to_sym]
			when v = s.scan(/==|:=/)
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
				@pos = 0
				['end', :eos]
			else
				puts "Unknown char '#{s.getch()}'"
				find_token
		end
	end

	def step_deindent
		raise 'Requires deindent' if tok != :deindent
		if @tok_val == 1
			@tok = nil
			step
		else
			@tok_val -= 1
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
		#puts "token: #{@tok}, #{@tok_val}\n#{source.format}"
	end
end
