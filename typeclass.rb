module TypeClass
	def self.is_instance?(args, input, inst)
		map = {}
		[can_be_instance?(args, map, input, inst), map]
	end
	
	def self.can_be_instance?(args, map, input, inst)
		input = input.prune
		inst = inst.prune
		
		if inst.is_a? Types::Param
			# TODO: Make sure constraints on inst are met
			
			if map.key? inst.param
				return map[inst.param] == input
			else
				map[inst.param] = input
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