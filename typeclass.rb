module TypeClass
	def self.find_instance(args, input)
		input.complex.instances.find do |inst|
			InferUtils.infer_type(inst, args)
			is_instance?(args, input, inst.itype.type)
		end
	end
	
	def self.is_instance?(args, input, inst)
		map = {}
		can_be_instance?(args, map, input, inst)
	end
	
	def self.can_be_instance?(args, map, input, inst)
		input = input.prune
		inst = inst.prune
		
		if inst.is_a? Types::Variable
			# TODO: Make sure constraints on inst are met
			
			if map.key? inst
				return map[inst] == input
			else
				map[inst] = input
				return true 
			end
		end
		
		return false if input.is_a? Types::Variable
		
		return true if inst == input
		
		inst_args = inst.type_args
		input_args = input.type_args
		
		return false if inst_args.size != input_args.size
		
		input_args.zip(inst_args).all? do |arg|
			can_be_instance?(args, map, arg.first, arg.last)
		end
	end
end