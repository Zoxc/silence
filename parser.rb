require_relative 'lexer'
require_relative 'ast'
require_relative 'print'

class Parser
	attr_reader :l
	
	def initialize(str)
		@src = str
		@l = Lexer.new(str)
	end
	
	def tok
		@l.tok
	end
	
	def tok_val
		@l.tok_val
	end
	
	def skip(v)
		step if v == tok
	end
	
	def step
		case tok
			when :line
				@l.step_line
			when :deindent
				@l.step_deindent
			else
				@l.step
		end
	end
	
	def eq(t, v = nil)
		if t == tok
			return if (v && v != tok_val)
			tok_val
		end
	end
	
	def matches(t, v = nil)
		if t == tok
			return if (v && v != tok_val)
			r = tok_val
			step
			r
		end
	end
	
	def expected(v)
		@l.expected v
	end
	
	def match(t, v = nil)
		if t == tok && (!v || v == tok_val)
			r = tok_val
			step
			r
		else
			expected @l.format_tok(t, v)
		end
	end
	
	def until_tok(t, v = nil)
		while !(tok == t && (!v || tok_val == v)) && tok != :eos
			yield
		end
	end
	
	def source
		src = @l.source
		r = yield src
		src.extend(@l.last_ended)
		r
	end
	
	def program
		r = AST::Program.new(AST::GlobalScope.new(global_scope_entries))
		expected('end') unless tok == :eos
		r
	end
	
	def parse_scope(baseline, &block)
		if eq :line
			if @l.indent_newline(baseline)
				r = yield
				match(:deindent)
				return r
			end
		end
		if matches(:sym, '{')
			r = yield
			match(:sym, '}')
			return r
		end
		[]
	end
	
	def global_scope(baseline)
		AST::GlobalScope.new(parse_scope(baseline) { global_scope_entries })
	end
	
	def global_scope_entries
		r = []
		skip :line
		while (entry = global_scope_entry)
			r << entry
			skip :line
		end
		r
	end
	
	def global_scope_entry
		case tok
			when :id
				case tok_val
					when :struct
						struct
					when :class
						self.class
					else
						field_or_func
				end
		end
	end
	
	def template_parameter
		source do |s|
			name = match :id
			AST::Template::Parameter.new(s, name, nil)
		end
	end
	
	def template_parameters
		r = []
		return r unless tok == :id
		
		loop do
			r << template_parameter
			if matches(:sym, ',')
				skip :line
			else
				return r
			end
		end
	end
	
	def class
		source do |s|
			baseline = @l.indent
			step
			name = match :id
			tp = template_parameters
			scope = global_scope(baseline)
			AST::Interface.new(s, name, scope, tp)
		end
	end
	
	def struct
		source do |s|
			baseline = @l.indent
			step
			name = match :id
			tp = template_parameters
			scope = global_scope(baseline)
			AST::Struct.new(s, name, scope, tp)
		end
	end
	
	def function_param
		source do |s|
			AST::Function::Parameter.new(s, match(:id), opt_type_specifier)
		end
	end
	
	def function_params
		r = []
		skip :line
		
		if tok == :id
			loop do
				r << function_param
				if matches(:sym, ',')
					skip :line
				else
					break
				end
			end
		end
		
		match :sym, ')'
		r
	end
	
	def function(s, baseline, name)
		params = pattern_factor
		
		type_check_s = nil
		type_check = nil
		source do |s|
			if matches(:sym, '::')
				type_check = type
				type_check_s = s
			end
		end
		
		group = group(baseline)
		
		func = AST::Function.new
		func.source = s
		func.name = name
		func.params = params
		func.attributes = []
		func.scope = group
		
		type_check ? AST::TypeCheckNode.new(type_check_s, func, type_check) : func
	end
	
	def field_or_func
		source do |s|
			baseline = @l.indent
			name = match :id
			if eq(:sym, ':')
				variable_decl s, name
			else
				function s, baseline, name
			end
		end
	end
	
	def type
		source do |s|
			if matches(:sym, '*')
				skip :line
				AST::PtrTypeNode.new(s, type)
			else
				AST::NamedTypeNode.new(s, match(:id), type_parameters)
			end
		end
	end
	
	def type_parameters
		return [] unless matches(:sym, '[')
		r = []
		skip :line
		
		loop do
			r << type
			if matches(:sym, ',')
				skip :line
			else
				break
			end
		end
		
		match :sym, ']'
		r
	end
	
	def opt_type_specifier
		type_specifier if eq :sym, ':'
	end
	
	def type_specifier
		match :sym, ':'
		skip :line
		type
	end
	
	def group(baseline)
		r = []
		if eq :line
			if @l.indent_newline(baseline)
				while is_expression do
					r << expression
					match :line unless eq :deindent
				end
				match(:deindent)
				return AST::LocalScope.new(r)
			end
		end
		if matches(:sym, '{')
			skip :line
			while is_expression do
				r << expression
				match :line unless eq :sym, '}'
			end
			match(:sym, '}')
		end
		AST::LocalScope.new(r)
	end
	
	def expression
		if tok == :id
			case tok_val
				when :return
					return _return
				when :if
					return _if
			end
		end
		
		pred_operator
	end
	
	def _return
		step
		skip :line
		AST::Return.new(expression)
	end
	
	def _if
		baseline = @l.indent
		step
		skip :line
		exp = expression
		grp = group(baseline)
		skip :line
		
		if eq :id, :else
			baseline = @l.indent
			step
			else_grp = group(baseline)
		end
		
		AST::If.new(exp, grp, else_grp)
	end
	
	Operators = {}
	pred = proc { |p, *args| args.each { |a| Operators[a] = p }}
	pred_num = 0
	pred.(pred_num += 1, '+=', '-=', '*=', '/=', '%=', '=')
	pred.(pred_num += 1, '+', '-')
	pred.(pred_num += 1, '*', '/', '%')
	
	def is_pred_op
		tok == :sym && Operators[tok_val]
	end
	
	def pred_operator(left = unary, min = 0)
		loop do
			break unless is_pred_op
			op = tok_val
			pred = Operators[tok_val]
			
			break if pred < min
			
			step
			
			source do |s|
				right = unary
				
				loop do
					break unless is_pred_op
					next_pred = Operators[tok_val]
					
					break if next_pred <= pred
					
					right = pred_operator(right, next_pred)
				end
				
				left = AST::BinOp.new(s, left, op.to_sym, right)
			end
		end
		
		left
	end
	
	def variable_decl(s, name)
		source do |ts|
			if matches(:sym, ':=')
				value = expression
			else
				type = type_specifier
				
				if matches(:sym, '=')
					skip :line
					value = expression
				end
			end
			
			AST::VariableDecl.new(s, ts, name, type, value)
		end
	end
	
	def unary
		apply
	end
	
	def is_expression
		case tok
			when :sym
				case tok_val
					when '('
						true
				end
			when :id, :int
				true
		end
	end
	
	def arguments
		r = []
		
		loop do
			r << expression
			if matches(:sym, ',')
				skip :line
			else
				break
			end
		end
	
		r
	end
	
	def apply()
		s = @l.source
		result = chain
		
		while is_expression
			new_s = @l.source
			args = [source { |s| new_src = s; chain }]
			s.extend(@l.last_ended)
			result = AST::Call.new(s, result, args)
			s = new_s
		end
		
		result
	end
	
	def chain
		result = factor
		
		while matches(:sym, '.')
			source do |s|
				skip :line
				result = AST::Field.new(s, result, match(:id))
			end
		end
		
		result
	end
	
	def tuple(s)
		step
		skip :line
		
		r = []
		if !eq(:sym, ')')
			loop do
				r << yield
				break unless matches :sym, ','
			end
		end
		
		match :sym, ')'
		
		if r.size == 1
			r.first
		else
			AST::Tuple.new(s, r)
		end
	end
	
	def pattern
		source do |s|
			factor = pattern_factor
			
			if factor && matches(:sym, '::')
				AST::TypeCheckNode.new(s, factor, type)
			else
				factor
			end
		end
	end
	
	def pattern_factor
		source do |s|
			case tok
				when :sym
					case tok_val
						when '('
							tuple(s) { pattern }
					end
				when :id
					AST::VariableRef.new(s, match(:id))
			end
		end
	end
	
	def factor
		source do |s|
			case tok
				when :sym
					case tok_val
						when '('
							tuple(s) { expression}
						else
							expected 'expression'
					end
				when :int
					AST::Literal.new(s, :int, match(:int))
				when :id
					case tok_val
						when :true
							step
							AST::Literal.new(s, :bool, true)
						when :false
							step
							AST::Literal.new(s, :bool, false)
						else
							name = match(:id)
							
							if tok == :sym && [':=', ':'].include?(tok_val)
								variable_decl(s, name)
							else
								AST::VariableRef.new(s, name)
							end
					end
				else
					expected 'expression'
			end
		end
	end
end
