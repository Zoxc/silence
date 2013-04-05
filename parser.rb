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
					when :instance
						instance
					when :class
						self.class
					when :type
						type_function
					else
						field_or_func
				end
		end
	end
	
	def type_param
		source do |s|
			name = match :id
			AST::TypeParam.new(s, name, nil)
		end
	end
	
	def type_params
		r = []
		return r unless tok == :id
		
		loop do
			r << type_param
			if matches(:sym, ',')
				skip :line
			else
				return r
			end
		end
	end
	
	def type_function
		source do |s|
			step
			name = match :id
			AST::TypeFunction.new(s, name)
		end
	end
	
	def class
		source do |s|
			baseline = @l.indent
			step
			name = match :id
			tp = type_params
			scope = global_scope(baseline)
			AST::TypeClass.new(s, name, scope, tp)
		end
	end
	
	def instance
		s = @l.source
		baseline = @l.indent
		step
		if matches(:sym, '[')
			skip :line
			tp = type_params
			match(:sym, ']')
		end
		type_class = source { |s| AST::NameRef.new(s, match(:id)) }
		args = type_parameters(false)
		s.extend(@l.last_ended)
		
		scope = global_scope(baseline)
		AST::TypeClassInstance.new(s, type_class, args, scope, tp || [])
	end
	
	def struct
		source do |s|
			baseline = @l.indent
			step
			name = match :id
			tp = type_params
			scope = global_scope(baseline)
			AST::Struct.new(s, name, scope, tp)
		end
	end
	
	def function_param(func)
		source do |s|
			AST::Function::Param.new(s, func, match(:id), opt_type_specifier)
		end
	end
	
	def function_params(func)
		r = []
		skip :line
		
		if tok == :id
			loop do
				r << function_param(func)
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
	
	def function(s, baseline, name, props)
		if matches(:sym, '[')
			skip :line
			tp = type_params
			match(:sym, ']')
		else
			tp = []
		end
		
		func = AST::Function.new
		
		match(:sym, '(')
		params = function_params(func)
		
		if matches :sym, '->'
			skip :line
			result = type
		end
		
		func.source = AST::Source.new(s.input, s.range)
		func.source.extend(@l.last_ended)
		
		group = group(baseline)
		
		func.name = name
		func.params = params
		func.type_params = tp
		func.result = result
		func.attributes = []
		func.scope = group
		func.props = props
		func
	end
	
	def properties
		props = {}
		case
			when matches(:id, :shared) 
				props[:shared] = true
			when matches(:id, :import) 
				props[:import] = true
			when matches(:id, :export) 
				props[:export] = true
			else
				return props
		end while true
	end
	
	def field_or_func
		source do |s|
			props = properties
			baseline = @l.indent
			name = match :id
			if eq(:sym, '[') ||  eq(:sym, '(')
				function s, baseline, name, props
			else
				variable_decl s, name, props
			end
		end
	end
	
	def type
		source do |s|
			arg = type_unary
			if matches(:sym, '->')
				skip :line
				AST::FunctionType.new(s, arg, type)
			else
				arg
			end
			
		end
	end
	
	def type_unary
		if matches(:sym, '*')
				skip :line
				AST::UnaryOp.new(s, :'*', type_chain)
		else
			type_chain
		end
	end
	
	def type_chain
		result = type_factor
		
		while eq(:sym, '.')
			source do |s|
				step
				name = match(:id)
				skip :line
				result = AST::Field.new(s, result, name)
			end
		end
		
		result
	end
	
	def type_factor
		source do |s|
			if eq(:sym, '(')
				tuple(s) { type }
			else
				var = source { |s| AST::NameRef.new(s, match(:id)) }
				
				if matches(:sym, '[')
					AST::Index.new(s, var, type_parameters)
				else
					var
				end
			end
		end
	end
	
	def type_parameters(terminate = true, expr = proc { type })
		r = []
		skip :line
		
		loop do
			r << expr.()
			if matches(:sym, ',')
				skip :line
			else
				break
			end
		end
		
		match(:sym, ']') if terminate
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
	
	def pred_operator(left = function_operator, min = 0)
		loop do
			break unless is_pred_op
			op = tok_val
			pred = Operators[tok_val]
			
			break if pred < min
			
			step
			skip :line
			
			source do |s|
				right = function_operator
				
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
	
	def variable_decl(s, name, props)
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
			
			AST::VariableDecl.new(s, ts, name, type, value, props)
		end
	end
	
	def function_operator
		source do |s|
			arg = unary
			if matches(:sym, '->') || matches(:sym, '->')
				skip :line
				AST::FunctionType.new(s, arg, function_operator)
			else
				arg
			end
			
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
		
		while tok == :sym && ['(', '.', '['].include?(tok_val)
			source do |s|
				case tok_val
					when '['
						step
						skip :line
						
						result = AST::Index.new(s, result, type_parameters(true, proc { expression }))
					when '('
						step
						skip :line
						if eq :sym, ')'
							args = []
						else
							args = arguments
						end
						result = AST::Call.new(s, result, args)
						match :sym, ')'
					when '.'
						step
						skip :line
						name = match(:id)
						result = AST::Field.new(s, result, name)
				end
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
			AST::Grouped.new(s, r.first)
		else
			AST::Tuple.new(s, r)
		end
	end
	
	def factor
		source do |s|
			case tok
				when :sym
					case tok_val
						when '('
							tuple(s) { expression }
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
								variable_decl(s, name, {})
							else
								AST::NameRef.new(s, name)
							end
					end
				else
					expected 'expression'
			end
		end
	end
end
