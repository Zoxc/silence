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
					when :when
						struct_when
					when :instance
						instance
					when :class
						self.class
					when :type
						type_function
					when :alias
						alias_type
					when :action
						action
					else
						field_or_func
				end
		end
	end
	
	def type_context
		if matches(:id, :where)
			r = []
			loop do
				r << expression
				if matches(:sym, ',')
					skip :line
				else
					return r
				end
			end
		else
			[]
		end
	end

	def kind_params
		source do |s|
			if matches(:sym, '[')
				skip :line
				params = type_params
				match(:sym, ']')
				AST::KindParams.new(s, params, type_context)
			else
				AST::KindParams.new(s, [], [])
			end
		end
	end
	
	def type_param
		source do |s|
			name = match(:id)
			kind = kind_params
			type, value = if matches(:sym, '::')
				[expression, true]
			else
				[opt_type_specifier, false]
			end
			AST::TypeParam.new(s, name, kind, type, value)
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
	
	def alias_type
		source do |s|
			step
			name = match :id
			match(:sym, '=')
			AST::TypeAlias.new(s, name, expression)
		end
	end
	
	def type_function
		source do |s|
			step
			name = match :id
			AST::TypeFunction.new(s, name, AST::KindParams.new(s, [], []), nil)
		end
	end
	
	def class
		source do |s|
			baseline = @l.indent
			step
			name = match :id
			tp = kind_params
			scope = global_scope(baseline)
			AST::TypeClass.new(s, name, scope, tp)
		end
	end
	
	def instance
		s = @l.source
		baseline = @l.indent
		step
		tp = kind_params
		type_class = source { |s| AST::NameRef.new(s, match(:id)) }
		args = type_parameters(false)
		s.extend(@l.last_ended)
		
		scope = global_scope(baseline)
		AST::TypeClassInstance.new(s, type_class, args, scope, tp)
	end
	
	def struct
		s = @l.source
		baseline = @l.indent
		step
		name = match :id
		tp = kind_params
		s.extend(@l.last_ended)
		scope = global_scope(baseline)
		AST::Struct.new(s, name, scope, tp)
	end
	
	def struct_when
		s = @l.source
		baseline = @l.indent
		step
		name = match :id
		tp = kind_params
		s.extend(@l.last_ended)
		scope = global_scope(baseline)
		AST::StructCase.new(s, name, scope, tp)
	end
	
	def action
		source do |s|
			step
			baseline = @l.indent
			type = match :id
			function(s, baseline, nil, {}, type)
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
	
	def function(s, baseline, name, props, action_type)
		tp = kind_params
		
		func = AST::Function.new(AST::Source.new(s.input, s.range), name, tp)
		
		if action_type && !eq(:sym, '(')
			params = []
		else
			action_type = "#{action_type}_args".to_sym if action_type
			match(:sym, '(')
			params = function_params(func)
		end
		
		if !action_type && matches(:sym, '->')
			skip :line
			result = expression
		end
		
		func.source.extend(@l.last_ended)
		
		group = group(baseline)
		
		func.action_type = action_type
		func.params = params
		func.result = result
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
			
			if (eq(:sym, '[') || eq(:sym, '(')) && !@l.whitespace
				function s, baseline, name, props, nil
			else
				props[:field] = true
				
				if matches(:sym, '=')
					skip :line
					val = expression
				else
					type = expression

					if matches(:sym, '=')
						skip :line
						val = expression
					end
				end
				AST::VariableDecl.new(s, name, type, val, props)
			end
		end
	end
	
	def type_parameters(terminate = true)
		r = []
		skip :line
		
		loop do
			r << expression
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
		pred_operator if is_expression
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
	
	def is_expression
		return true if is_factor
		case tok
			when :id
				case tok_val
					when :return, :if
						true
				end
			when :sym
				case tok_val
					when '*', '&', '.'
						true
				end
		end
	end
	
	def expression
		if tok == :id
			case tok_val
				when :return
					return _return
				when :if
					return _if
			end
		elsif eq(:sym, '.')
			return variable_decl
		end
		
		assign_operator
	end
	
	def variable_decl
		source do |s|
			step
			skip :line
			
			name = match :id
			type = opt_type_specifier
			value = nil
		
			if matches(:sym, '=')
				skip :line
				value = expression
			end
		
			AST::VariableDecl.new(s, name, type, value, [])
		end
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
	
	def assign_operator
		result = pred_operator
		
		case tok_val
			when '+=', '-=', '*=', '/=', '%=', '='
				src = @l.source
				op = tok_val
				step
				skip :line
				return AST::BinOp.new(src, result, op, assign_operator)
		end if tok == :sym
		
		result
	end
	
	Operators = {}
	pred = proc { |p, *args| args.each { |a| Operators[a] = p }}
	pred_num = 0
	pred.(pred_num += 1, '->')
	pred.(pred_num += 1, '+', '-', '~')
	pred.(pred_num += 1, '*', '/', '%')
	
	def is_pred_op
		tok == :sym && Operators[tok_val]
	end
	
	def pred_operator(left = unary, min = 0)
		loop do
			break unless is_pred_op
			op = tok_val
			op_src = @l.source
			pred = Operators[tok_val]
			
			break if pred < min
			
			step
			skip :line
			
			right = unary
			
			loop do
				break unless is_pred_op
				next_pred = Operators[tok_val]
				
				break if next_pred <= pred
				
				right = pred_operator(right, next_pred)
			end
			
			left = AST::BinOp.new(op_src, left, op, right)
		end
		
		left
	end
	
	def unary
		if tok == :sym and ['&', '*'].include?(tok_val)
			source do |s|
				op = tok_val
				step
				skip :line
				AST::UnaryOp.new(s, op, unary)
			end
		else
			apply
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
		
		while is_factor
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
						
						result = AST::Index.new(s, result, type_parameters(true))
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
				r << expression
				if matches :sym, ','
					skip :line
				else
					break
				end
			end
		end
		
		match :sym, ')'
		
		if r.size == 1
			AST::Grouped.new(s, r.first)
		else
			AST::Tuple.new(s, r)
		end
	end
	
	def array(s)
		step
		skip :line
		
		r = []
		if !eq(:sym, ']')
			loop do
				r << expression
				if matches :sym, ','
					skip :line
				else
					break
				end
			end
		end
		
		match :sym, ']'
		
		AST::Array.new(s, r)
	end
	
	def is_factor
		case tok
			when :sym
				case tok_val
					when '[', '('
						true
				end
			when :id, :int, :str
				true
		end
	end
	
	def factor
		source do |s|
			case tok
				when :sym
					case tok_val
						when '['
							array(s)
						when '('
							tuple(s)
						else
							expected 'expression'
					end
				when :int
					AST::Literal.new(s, :int, match(:int))
				when :str
					AST::Literal.new(s, :string, match(:str))
				when :id
					case tok_val
						when :typeof
							step
							AST::TypeOf.new(s, chain)
						when :true
							step
							AST::Literal.new(s, :bool, true)
						when :false
							step
							AST::Literal.new(s, :bool, false)
						else
							AST::NameRef.new(s, match(:id))
					end
				else
					expected 'expression'
			end
		end
	end
end
