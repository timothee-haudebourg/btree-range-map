#!/usr/bin/env ruby

class Type
	attr_accessor :size, :name

	def initialize(signed, size)
		@signed = signed
		@size = size

		if self.signed? then
			@name = "i#{@size}"
		else
			@name = "u#{@size}"
		end
	end

	def min
		if self.signed? then
			- self.max
		else
			0
		end
	end

	def max
		(1 << self.unsigned_size) - 1
	end

	def signed?
		@signed
	end

	def unsigned_size
		if self.signed? then
			@size - 1
		else
			@size
		end
	end

	def to_s
		self.name
	end
end

$types = [
	Type.new(false, 8),
	Type.new(false, 16),
	Type.new(false, 32),
	Type.new(false, 64),
	Type.new(false, 128),
	Type.new(true, 8),
	Type.new(true, 16),
	Type.new(true, 32),
	Type.new(true, 64),
	Type.new(true, 128)
]

def impl(ty1, ty2, gate = nil)
	max = [ty2.max - ty1.min, ty1.max - ty2.min].max
	output_min_size = max.bit_length
	len_min_size = (max+1).bit_length # the `Len` type must be able to store the total number of elements.
	len_size = [8, 16, 32, 64, 128].find { |s| s >= len_min_size }

	intermediate_signed = ty1.signed? || ty2.signed?
	intermediate_min_size = output_min_size
	intermediate_min_size += 1 if intermediate_signed
	intermediate_size = [8, 16, 32, 64, 128].find { |s| s >= intermediate_min_size }

	unless intermediate_size.nil? || len_size.nil? then
		intermediate_ty = Type.new(intermediate_signed, intermediate_size)
		len_ty = Type.new(false, len_size)

		if ty1 == ty2 then
			puts gate unless gate.nil?
			puts "impl_measure!(@refl #{ty1}, #{intermediate_ty}, #{len_ty});"
		else
			puts gate unless gate.nil?
			puts "impl_measure!(@both #{ty1}, #{ty2}, #{intermediate_ty}, #{len_ty});"
		end
	end
end

$types.each do |ty|
	impl(ty, ty)
end

$types.combination(2).map do |ty1, ty2|
	impl(ty1, ty2)
end

[8, 16, 32, 64].each do |word_size|
	usize = Type.new(false, word_size)
	usize.name = "usize"

	gate = "#[cfg(target_pointer_width=\"#{word_size}\")]"

	impl(usize, usize, gate)

	$types.each do |ty|
		impl(usize, ty, gate)
	end
end