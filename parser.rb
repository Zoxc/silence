require_relative 'lexer'
require_relative 'ast'
require_relative 'print'

class Parser
	attr_reader :l, :imports
	
	def initialize(str)
		@l = Lexer.new(str)
		@imports = []
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
		r = global_scope_entries(proc { eq(:eos) })
		expected('end') unless tok == :eos
		r
	end

	def can_be_scope
		eq(:line) || eq(:sym, '{')
	end
	
	def parse_scope(baseline)
		if eq :line
			if @l.indent_newline(baseline)
				r = yield(proc { eq(:deindent) })
				match(:deindent)
				return r
			end
		end
		state = @l.dup
		skip :line
		if matches(:sym, '{')
			skip :line
			r = yield(proc { eq(:sym, '}') })
			match(:sym, '}')
			return r
		else
			@l = state
			nil
		end
	end
	
	def global_scope(baseline)
		AST::GlobalScope.new(parse_scope(baseline) { |term| global_scope_entries(term) } || [])
	end
	
	def global_scope_entries(term)
		r = []
		while (entry = global_scope_entry)
			r << entry if entry != true
			break if term.()
			match(:line)
		end
		r
	end
	
	def global_scope_entry
		case tok
			when :id
				case tok_val
					when :use
						step
						@imports << match(:str)
						true
					when :enum
						enum
					when :struct
						struct
					when :when
						struct_when
					when :instance
						instance
					when :class
						self.class
					when :type_func
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
				r << type_expression
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

	def is_type_expression
		return true if is_type_factor
		case tok
			when :sym
				case tok_val
					when '*'
						true
				end
		end
	end
	
	def type_expression
		type_operator
	end

	def type_operator(left = type_unary)
		while eq(:sym, '->')
			op_src = @l.source
			
			step
			skip :line
			
			left = AST::BinOp.new(op_src, left, '->', type_unary)
		end
		
		left
	end
	
	def type_unary
		if eq(:sym, '*')
			source do |s|
				op = tok_val
				step
				skip :line
				AST::UnaryOp.new(s, op, type_unary)
			end
		else
			type_apply
		end
	end
	
	def type_apply
		s = @l.source
		result = type_chain
		
		while is_type_factor
			new_s = @l.source
			args = [source { |s| new_src = s; type_chain }]
			s.extend(@l.last_ended)
			result = AST::Call.new(s, result, args)
			s = new_s
		end
		
		result
	end
	
	def type_chain
		result = type_factor

		while tok == :sym && ['(', '.'].include?(tok_val)
			source do |s|
				case tok_val
					when '('
						step
						skip :line
						
						result = AST::Index.new(s, result, type_parameters)
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
	
	def is_type_factor
		case tok
			when :sym
				case tok_val
					when '('
						true
				end
			when :id, :int, :str
				true
		end
	end
	
	def type_factor
		source do |s|
			case tok
				when :sym
					case tok_val
						when '('
							tuple(s)
						else
							expected 'type'
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
						else
							AST::NameRef.new(s, match(:id))
					end
				else
					expected 'type'
			end
		end
	end

	def kind_params(f = '(', l = ')')
		source do |s|
			if matches(:sym, f)
				skip :line
				params = type_params
				match(:sym, l)
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
				[type_expression, true]
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
			AST::TypeAlias.new(s, name, type_expression)
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
		s = @l.source
		baseline = @l.indent
		step
		name = match :id
		tp = kind_params
		s.extend(@l.last_ended)
		scope = global_scope(baseline)
		AST::TypeClass.new(s, name, scope, tp)
	end
	
	def instance
		s = @l.source
		baseline = @l.indent
		step
		tp = kind_params
		type_class = source { |s| AST::NameRef.new(s, match(:id)) }
		match(:sym, '(')
		args = type_parameters
		match(:sym, ')')
		s.extend(@l.last_ended)
		
		scope = global_scope(baseline)
		AST::TypeClassInstance.new(s, type_class, args, scope, tp)
	end
	
	def enum
		s = @l.source
		baseline = @l.indent
		step
		name = match :id
		s.extend(@l.last_ended)

		values = []

		result = AST::Enum.new(s, name, values)

		parse_scope(baseline) do |term|
			while eq(:id) do
				source do |s|
					values << AST::EnumValue.new(s, match(:id), result)
				end
				break if term.()
				match(:line)
			end
		end

		result
	end
	
	def struct
		s = @l.source
		baseline = @l.indent
		step
		ref = case  
			when matches(:id, :ref)
				:sizeable
			when matches(:id, :bare)
				:opaque
			else
				:copyable
		end
		name = match :id
		tp = kind_params
		s.extend(@l.last_ended)
		scope = if ref == :opaque
				AST::GlobalScope.new([])
			else
				global_scope(baseline)
			end
		AST::Struct.new(s, name, scope, tp, ref)
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
		
		r
	end
	
	def function(s, baseline, name, props, action_type)
		tp = kind_params('[', ']')
		
		func = AST::Function.new(AST::Source.new(s.input, s.range), name, tp)
		
		if action_type && !eq(:sym, '(')
			params = []
		else
			action_type = "#{action_type}_args".to_sym if action_type
			match(:sym, '(')
			params = function_params(func)
			match :sym, ')'
		end

		if action_type
			if matches(:sym, ':')
				skip :line

				func.init_list = []

				loop do
					source do |s|
						field = match(:id)
						match(:sym, '<-')
						func.init_list << AST::InitEntry.new(s, field, expression)
					end

					if matches(:sym, ',')
						skip :line
					else
						break
					end
				end
			end
		else
			if matches(:sym, '->')
				skip :line
				result = type_expression
			end
		end
		
		func.source.extend(@l.last_ended)
		
		group = group(baseline, AST::FuncScope)
		
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
					type = type_expression

					if matches(:sym, '=')
						skip :line
						val = expression
					end
				end
				AST::VariableDecl.new(s, name, type, val, props)
			end
		end
	end
	
	def type_parameters
		r = []
		skip :line
		
		loop do
			r << type_expression
			if matches(:sym, ',')
				skip :line
			else
				break
			end
		end
		
		r
	end
	
	def opt_type_specifier
		type_expression if is_type_expression
	end
	
	def group(baseline, scope = AST::LocalScope)
		r = []
		parse_scope(baseline) do |term|
			while is_expression do
				r << expression
				break if term.()
				match(:line) 
			end
		end
		scope.new(r)
	end

	def expression_group(baseline)
		source do |s|
			AST::ExpressionGroup.new(s, group(baseline))
		end
	end
	
	def is_expression
		return true if is_factor
		case tok
			when :id
				case tok_val
					when :return, :if, :match
						true
				end
			when :sym
				case tok_val
					when '*', '&', '.', '-', '+'
						true
				end
		end
	end
	
	def expression
		if tok == :id
			case tok_val
				when :match
					return _match
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
	
	def _match
		source do |s|
			baseline = @l.indent
			step
			skip :line
			exp = chain
			if matches(:id, :as)
				binding_source, binding = source do |s|
					next s, match(:id)
				end
			end

			else_group = nil
			whens = []

			parse_scope(baseline) do |term|
				while eq(:id, :when) do
					source do |s|
						when_baseline = @l.indent
						step
						skip :line
						when_type = expression
						when_group = expression_group(when_baseline)

						whens << AST::MatchWhen.new(s, when_type, when_group)
					end

					break if term.()
					match(:line)
				end

				else_group = if eq(:id, :else)
					else_baseline = @l.indent
					step
					g = expression_group(else_baseline)
					match(:line) unless term.()
					g
				end
			end

			if binding
				AST::MatchAs.new(s, exp, AST::MatchCases.new(s, binding_source, binding, whens, else_group))
			else
				AST::Match.new(s, exp, whens, else_group)
			end
		end
	end
	
	def _return
		source do |s|
			step
			skip :line
			AST::Return.new(s, expression)
		end
	end
	
	def _if
		baseline = @l.indent
		src, exp = source do |s|
			step
			skip :line
			[s, expression]
		end
		grp = expression_group(baseline)

		state = @l.dup
		skip :line
		
		if eq :id, :else
			baseline = @l.indent
			step
			else_grp = expression_group(baseline)
		else
			@l = state
		end
		
		AST::If.new(src, exp, grp, else_grp)
	end
	
	def assign_operator
		result = pred_operator
		
		case tok_val
			when *Core::AssignOps
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
	pred.(pred_num += 1, '==', '!=')
	pred.(pred_num += 1, '<', '>', '>=', '<=')
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
		if tok == :sym and ['&', '*', '-', '+', '!'].include?(tok_val)
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
						
						result = AST::Index.new(s, result, type_parameters)
						match(:sym, ']')
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

	def lambda
		baseline = @l.indent
		lambda = nil

		source do |s|
			lambda = AST::Lambda.new(s)
			matches(:sym, '->')


			if matches(:sym, '|')
				@in_lambda_params = true
				lambda.params = function_params(lambda)
				match(:sym, '|')
				@in_lambda_params = false
			else
				lambda.params = []
			end
		end

		lambda.scope = if can_be_scope
			group(baseline, AST::FuncScope)
		else
			source do |s|
				AST::FuncScope.new([AST::Return.new(s, expression)])
			end
		end

		lambda
	end
	
	def is_factor
		case tok
			when :sym
				case tok_val
					when '|'
						!@in_lambda_params
					when '[', '(', '|', '->'
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
						when '->', '|'
							lambda
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
						else
							AST::NameRef.new(s, match(:id))
					end
				else
					expected 'expression'
			end
		end
	end
end
