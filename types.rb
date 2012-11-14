
module Types
	class Type
		attr_accessor :source
		
		def text
			"<error>"
		end
		
		def real_text
			text
		end
		
		def args
			[]
		end
		
		def source_dup(source)
			result = dup
			result.source = source
			result
		end
	end

	class Fixed < Type
		attr_accessor :name
		
		def initialize(source, name)
			@source = source
			@name = name
		end
		
		def text
			@name
		end
	end

	class Variable < Type
		attr_accessor :instance
		
		def initialize(source, system, instance)
			@source = source
			@system = system
			@instance = instance
		end
		
		def text
			if @instance
				@instance.text
			else
				@name ||= @system.new_var_name
			end
		end
		
		def real_text
			"#{@name ||= @system.new_var_name} {#{text}}"
		end
		
		def source
			if @instance
				@instance.source
			else
				@source
			end
		end
	end

	class Function < Type
		attr_accessor :result
		
		def initialize(source, args, result)
			@source = source
			@args = args
			@result = result
		end
		
		def name
			:func
		end
		
		def text
			"(#{@args.map(&:text).join(", ")}): #{@result.text}"
		end
		
		def args
			[*@args, @result]
		end
		
		def args_dup(*args, result)
			Function.new(@source, args, result)
		end
	end
	
	class Struct < Type
		attr_accessor :struct
		
		def initialize(source, struct)
			@source = source
			@struct = struct
		end
		
		def name
			:struct
		end
		
		def text
			@struct.name
		end
	end
	
	def self.declare_type(name)
		AST::BuiltinScope.declare(name, Fixed.new(nil, name))
	end
	
	IntType = declare_type(:int)
	UnitType = declare_type(:unit)
	BoolType = declare_type(:bool)
	StringType = declare_type(:string)
	CharType = declare_type(:char)
end
