require_relative 'lexer'
require_relative 'ast'
require_relative 'print'

class Parser
	attr_reader :l, :imports
	
	AssignOps = ['+=', '-=', '*=', '/=', '%=', '~=', '^=', '&=', '|=', '=']
	
	def initialize(str)
		@l = Lexer.new(str)
		@imports = []
	end

	def push_state(n, v)
		old = instance_variable_get(n)
		instance_variable_set(n, v)
		r = yield
		instance_variable_set(n, old)
		r
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
		skip :line
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
					when :data
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
	
	def type_context_list
		r = []
		loop do
			r << type_expression
			if matches(:sym, ',')
				skip :line
			else
				break
			end
		end if is_type_expression
		r
	end

	def type_context
		if matches(:id, :where)
			type_context_list
		else
			[]
		end
	end

	def is_type_expression
		is_type_factor
	end
	
	def type_expression
		type_unary
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
		r = type_chain

		while eq(:sym, '*')
			source do |s|
				step
				r = AST::UnaryOp.new(s, '*', r)
			end
		end

		r
	end
	
	def type_chain
		result = type_factor

		while tok == :sym && ['[', '.'].include?(tok_val)
			source do |s|
				case tok_val
					when '['
						step
						skip :line
						
						result = AST::Index.new(s, result, type_parameters)
						match :sym, ']'
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
	
	def type_tuple(s)
		step
		skip :line
		
		r = []
		if !eq(:sym, ')')
			loop do
				r << type_operator
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
			AST::TypeTuple.new(s, r)
		end
	end
	
	def type_factor
		source do |s|
			case tok
				when :sym
					case tok_val
						when '('
							type_tuple(s)
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

	def kind_params
		source do |s|
			if matches(:sym, '[')
				skip :line
				values = {}
				params = type_params(values)
				match(:sym, ']')
				AST::KindParams.new(s, params, type_context, values)
			else
				AST::KindParams.new(s, [], [])
			end
		end
	end
	
	def type_param(values)
		source do |s|
			type = type_expression
			value = false

			if eq(:id)
				name = match(:id)
			elsif matches(:sym, '::')
				skip :line
				name = match(:id)
				value = true
			else
				raise CompileError, "Expected variable name\n#{type.source.format}" unless type.is_a?(AST::NameRef)
				name = type.name
				type = nil
			end

			kind = kind_params
			default = if matches(:sym, '=')
				skip :line
				source do |s|
					[s, value ? expression : type_expression]
				end
			end
			p = AST::TypeParam.new(s, name, kind, type, value)
			values[p] = AST::TypeParamValue.new(default.first, p, default.last) if default
			p
		end
	end
	
	def type_params(values)
		r = []
		return r unless tok == :id
		
		loop do
			r << type_param(values)
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
			AST::TypeFunction.new(s, name, AST::KindParams.new(s, [], []), (type_expression if is_type_expression))
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
		match(:sym, '[')
		args = type_parameters
		match(:sym, ']')
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
		sizeable = !matches(:id, :ref)
		name = match :id
		tp = kind_params
		s.extend(@l.last_ended)
		scope = global_scope(baseline)
		AST::Struct.new(s, name, scope, tp, sizeable)
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
			AST::Function::Param.new(s, func, *name_opt_type)
		end
	end
	
	def function_params(func)
		r = []
		skip :line
		
		if is_type_expression
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
		tp = kind_params
		
		func = AST::Function.new(AST::Source.new(s.input, s.range), name, tp)

		if matches(:sym, '*')
			source do |s|
				func.params = [AST::Function::Param.new(s, func, *name_opt_type)]
			end
			func.var_arg = true
		else
			if action_type && !eq(:sym, '(')
				func.params = []
			else
				action_type = "#{action_type}_args".to_sym if action_type
				match(:sym, '(')
				func.params = function_params(func)
				match :sym, ')'
			end
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
				
				if matches(:id, :constraints)
					constraints = type_context_list
				end
			end
		end
		
		func.source.extend(@l.last_ended)
		
		group = group(baseline, AST::FuncScope)
		
		func.action_type = action_type
		func.result = result
		func.scope = group
		func.props = props
		func.constraints = constraints
		func
	end
	
	def properties
		props = {attrs: []}
		case
			when matches(:id, :shared) 
				props[:shared] = true
			when matches(:id, :import) 
				props[:import] = true
			when matches(:id, :export) 
				props[:export] = true
			else
				if matches(:sym, '[')
					skip :line
					
					attrs = []

					if !eq(:sym, ']')
						loop do
							attrs << match(:id)
							if matches :sym, ','
								skip :line
							else
								break
							end
						end
					end
					
					match(:sym, ']')

					props[:attrs] = attrs
				end
				return props
		end while true
	end
	
	def field_or_func
		source do |s|
			props = properties
			baseline = @l.indent

			if matches(:id, :fn)
				function s, baseline, match(:id), props, nil
			else
				name, type = name_opt_type
				props[:field] = true
				
				if matches(:sym, '=')
					skip :line
					val = expression
				end

				AST::VariableDecl.new(s, name, type, val, props)
			end
		end
	end
	
	def type_parameters
		r = []
		skip :line
		
		loop do
			r << type_operator
			if matches(:sym, ',')
				skip :line
			else
				break
			end
		end
		
		r
	end
	
	def name_opt_type
		type = type_expression

		if eq(:id)
			name = match(:id)
		else
			raise CompileError, "Expected variable name\n#{type.source.format}" unless type.is_a?(AST::NameRef)
			name = type.name
			type = nil
		end

		return name, type
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
					when :return, :if, :match, :loop, :and, :or, :as, :var
						true
				end
			when :sym
				case tok_val
					when '*', '&', '.', '-', '+', '!', '~'
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
				when :break
					return _break
				when :loop
					return _loop
				when :if
					return _if
				when :var
					return variable_decl
			end
		end
		
		assign_operator
	end
	
	def variable_decl
		source do |s|
			step
			skip :line
			
			name, type = name_opt_type
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
			exp = push_state(:@in_match, true) { expression }
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
						when_cases = expression_list
						when_group = expression_group(when_baseline)

						whens << AST::MatchWhen.new(s, when_cases, when_group)
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
			AST::Return.new(s, is_expression ? expression : nil)
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
			else_grp = if eq :id, :if
				source do |s|
					AST::ExpressionGroup.new(s, AST::LocalScope.new([_if]))
				end
			else
				expression_group(baseline)
			end
		else
			@l = state
		end
		
		AST::If.new(src, exp, grp, else_grp)
	end
	
	def _break
		src = @l.source
		step
		AST::Break.new(src)
	end
	
	def _loop
		baseline = @l.indent
		src = @l.source
		step
		grp = expression_group(baseline)

		AST::Loop.new(src, grp)
	end
	
	def assign_operator
		result = pred_operator
		
		case tok_val
			when *AssignOps
				src = @l.source
				op = tok_val
				step
				skip :line
				return AST::BinOp.new(src, result, op, expression)
		end if tok == :sym
		
		result
	end
	
	Operators = {}
	pred = proc { |p, *args| args.each { |a| Operators[a] = p }}
	pred_num = 0
	pred.(pred_num += 1, :or)
	pred.(pred_num += 1, :and)
	pred.(pred_num += 1, '==', '!=')
	pred.(pred_num += 1, '|')
	pred.(pred_num += 1, '^')
	pred.(pred_num += 1, '&')
	pred.(pred_num += 1, '<', '>', '>=', '<=')
	pred.(pred_num += 1, '<<', '>>')
	pred.(pred_num += 1, '+', '-', '~')
	pred.(pred_num += 1, '*', '/', '%')
	
	def is_pred_op
		[:sym, :id].include?(tok) && Operators[tok_val]
	end
	
	def pred_operator(left = type_assert, min = 0)
		loop do
			break unless is_pred_op
			op = tok_val
			op_src = @l.source
			pred = Operators[tok_val]
			
			break if pred < min
			
			step
			skip :line
			
			right = type_assert
			
			loop do
				break unless is_pred_op
				next_pred = Operators[tok_val]
				
				break if next_pred <= pred
				
				right = pred_operator(right, next_pred)
			end
			
			left = AST::BinOp.new(op_src, left, op.to_s, right)
		end
		
		left
	end
	
	def type_assert
		r = unary
		if eq(:sym, '::')
			source do |s|
				step
				skip :line
				AST::TypeAssert.new(s, r, type_expression)
			end
		else
			r
		end
	end
	
	def unary
		if tok == :sym and ['&', '*', '-', '+', '!', '~'].include?(tok_val)
			source do |s|
				op = tok_val
				step
				skip :line
				AST::UnaryOp.new(s, op, unary)
			end
		else
			subscript
		end
	end

	def subscript
		r = apply
		if eq(:sym, '\'')
			source do |s|
				step
				skip :line
				AST::Subscript.new(s, r, subscript)
			end
		else
			r
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

	def expression_list
		r = []
		loop do
			r << expression
			if matches :sym, ','
				skip :line
			else
				break
			end
		end
		r
	end
	
	def tuple(s)
		step
		skip :line
		
		r = []
		r = expression_list if !eq(:sym, ')')
		
		match :sym, ')'
		
		if r.size == 1
			AST::Grouped.new(s, r.first)
		else
			AST::ValueTuple.new(s, r)
		end
	end
	
	def array(s)
		step
		skip :line
		
		r = []
		r = expression_list if !eq(:sym, ']')
		
		match :sym, ']'
		
		AST::Array.new(s, r)
	end

	def lambda
		baseline = @l.indent
		r = nil

		source do |s|
			r = AST::Lambda.new(s)
			r.params = matches(:sym, '\\') ? function_params(r) : []
			match(:sym, '->')
		end

		r.scope = if can_be_scope
			group(baseline, AST::FuncScope)
		else
			source do |s|
				AST::FuncScope.new([AST::Return.new(s, expression)])
			end
		end

		r
	end
	
	def is_factor
		case tok
			when :sym
				case tok_val
					when '[', '(', '->', '\\'
						true
				end
			when :id
				case tok_val
					when :or, :and
						false
					when :as
						!@in_match 
					else
						true
				end
			when :int, :str
				true
		end
	end
	
	def factor
		source do |s|
			case tok
				when :sym
					case tok_val
						when '->', '\\'
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
						when :nil
							step
							AST::Literal.new(s, :nil, nil)
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
