module TypeClass
	def self.is_instance?(args, input, inst)
		map = {}
		
		result = Types.cmp_types(input, inst) do |input, inst|
			if inst.is_a? Types::Param
				# TODO: Make sure constraints on inst are met
				
				if map.key? inst.param
					[true, map[inst.param] == input]
				else
					map[inst.param] = input
					[true, true]
				end
			else
				[input.is_a?(Types::Variable), false]
			end
		end
		
		[result, map]
	end
end