require 'strscan'

class Lexer
	class Error < Exception
	end
	
	attr_reader :pos, :tok, :tok_str, :tok_val, :last_ended
	
	def initialize(str)
		@src = str
		@last_pos = 0
		@pos = 0
		@tok_str = ''
		@scanner = StringScanner.new(str)
		step
	end
	
	def format_tok t, v
		v ? "#{v.inspect} (#{t})" : t.to_s
	end
	
	def source
		AST::Source.new(@src, (@pos)...(@pos + @tok_str.size))
	end
	
	def expected(v)
		raise Error, "Expected #{v}, but found #{format_tok(@tok, @tok_val)}\n#{source.format}"
	end
	
	def find_token
		s = @scanner
		@pos = @scanner.pos
		case
			when v = s.scan(/[ \t]+/)
				find_token
			when v = s.scan(/((\r\n|\n|\r)[ \t]*)+/)
				[v, :line]
			when v = s.scan(/[0-9]+/)
				[v, :int, v.to_i]
			when v = s.scan(/[A-Za-z_]+\w*/)
				[v, :id, v.to_sym]
			when v = s.scan(/==|:=/)
				[v, :sym, v]
			when v = s.scan(/[()\[\]{}\.*+=\-,:]/)
				[v, :sym, v]
			when s.eos?
				@pos = 0
				['end', :eos]
			else
				c = s.getch()
				puts "Unknown char '#{c}'"
				find_token
		end
	end
	
	def step
		@last_ended = @pos + @tok_str.size
		raise if @token == :eos
		@tok_str, @tok, @tok_val = find_token
		#puts "token: #{@tok}, #{@tok_val}\n#{source.format}"
	end
end
