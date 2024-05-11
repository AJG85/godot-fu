# CBOR.gd
# Utility script for concise binary object respresentation
#
# References:
# https://datatracker.ietf.org/doc/html/rfc7049 (proposal circa 2013)
# https://datatracker.ietf.org/doc/html/rfc8949 (standard circa 2020)
#
# Notes:
#
# Initially [PoolByteArray] was selected as the type interface for three reasons:
# 1. It has methods for compression, useful for transmitting wire formats like CBOR
# 2. It has methods for base16 encoding, hexadecimal representation is common for CBOR
# 3. It was the only contiguous [Array] type for "byte data" I knew of in Godot 3.x
#
# Three things lacking that required workarounds:
# 1. hex_decode - apparently no one ever wanted to "unhash" back to raw bytes?
# 2. byte - the smallest type in [GDScript] is actually 64-bit despite uint8_t in C++?
# 3. float - [GDScript] only supports the equivalent of a 64-bit double in C++?
#
# Three types that did not work:
# [StreamPeerBuffer] (really [StreamPeer] base class) appended bytes instead of bitwise ops!
# [File] has a similar looking byte stream R/W interface but closely coupled to disk I/O!
# [Array] is too generic with lots of helper methods that do not help for CBOR!
#
# Almost all wire formats, including CBOR, use network byte order aka big-endian
# Almost all desktops, laptops, etc use x86_64 which is typically little-endian
# Mobile and tablet devices are usually ARM which actually supports both
# i.e. we need to detect "endianess" and reverse bytes occasionally!
#
# While CBOR itself is concise the ICD is quite verbose
# Given that it took the better part of a decade to ratify ...
# We can assume "works on my machine" for a few use cases is insufficient
# i.e. we should test on multiple platforms before releasing into the wild!
#
class_name CBOR
extends GDScript
# License: MIT
# Reference: https://en.wikipedia.org/wiki/MIT_License
###############################################################################
# Copyright (c) 2023 Geek Gang, LLC
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
###############################################################################
const semver = "1.0.0" # bump me accordingly https://semver.org
###############################################################################
# - VERSION - | - AUTHOR - | -------------- SUMMARY OF CHANGES -------------- |
#    1.0.0    |  DinoDude  |  Initial implementation of encode, decode, etc   |
###############################################################################
#--------------------------------------- START API
# HEY YOU! Do you like computer science even more than video games?
# If so, please consider making a pull request to improve this for posterity.
# You can report issues and bugs but it is better to fix errors yourself!
# GitHub: https://www.github.com/AJG85/godot-fu
#--------------------------------------- TESTING
static func test():
	# todo: add more test cases or stream JSON vector?
	# https://github.com/cbor/test-vectors/blob/master/appendix_a.json
	print('CBOR.gd version ', semver)
	
	var test = 42
	var res = hex_encode(encode(test))
	print(res) # 182a
	
	test = '0x182A'
	res = decode(hex_decode(test))
	print(res) # 42
	
	test = -123456789
	res = hex_encode(encode(test))
	print(res) # 3a075bcd14
	
	test = '3a075bcd14'
	res = decode(hex_decode(test))
	print(res) # -123456789
	
	test = 1_000_000_000_000
	res = hex_encode(encode(test))
	print(res) # 1b000000e8d4a51000
	
	test = '1b000000e8d4a51000'
	res = decode(hex_decode(test))
	print(res) # 1000000000000
	
	test = 'a16568656c6c6f65776f726c64'
	res = decode(hex_decode(test))
	print(res) # {hello:world}
	
	# todo: fixme!
	test = 3.14
	res = hex_encode(encode(test))
	print(res) # fb40091eb851eb851f
	
	# todo: fixme!
	test = 'fb40091eb851eb851f'
	res = decode(hex_decode(test))
	print(res) # 3.14
	
	test = 'ggEC'
	res = decode(Marshalls.base64_to_raw(test))
	print(res) # [1, 2]
	
	test = 'ggECggMEggUG'
	res = decode(Marshalls.base64_to_raw(test))
	print(res) # [[1, 2], [3, 4], [5, 6]]
	
	test = PoolByteArray([0xA2, 0x67, 0x63, 0x6F, 0x6D, 0x70, 0x61, 0x63, 0x74, 0xF5, 0x66, 0x73, 0x63, 0x68, 0x65, 0x6D, 0x61, 0x00])
	res = decode(test)
	print(res) # {compact:True, schema:0}
	
	test = '82a26161f461626163a26164016165a1616620'
	res = decode(hex_decode(test))
	print(res) # [{a:False, b:c}, {d:1, e:{f:-1}}]
#--------------------------------------- ENCODE
static func encode(anything) -> PoolByteArray:
	var helper = CBORWriter.new(anything)
	return helper.write_all()
#--------------------------------------- DECODE
static func decode(cbor: PoolByteArray) -> Array:
	var helper = CBORReader.new(cbor)
	return helper.read_all()
#--------------------------------------- HEX ENCODE
static func hex_encode(cbor: PoolByteArray) -> String:
	return cbor.hex_encode()
#--------------------------------------- HEX DECODE
static func hex_decode(hex: String) -> PoolByteArray:
	var bytes = PoolByteArray()
	var prefix = '0x'
	var tmp = hex.to_lower()
	tmp = tmp.trim_prefix(prefix)
	if tmp.empty():
		return bytes
	if tmp.is_valid_hex_number():
		bytes.resize(tmp.length() / 2)
		for idx in bytes.size():
			bytes[idx] = (prefix + tmp.substr(idx*2, 2)).hex_to_int()
	return bytes
#--------------------------------------- END API


########################################
######## Implementation Details ########
########################################
# base class for shared functionality
class CBORBase:
	# Signals
	signal invalid_format(what, why) # CBOR is not well-formed, cannot parse format!
	signal invalid_data(why)         # CBOR is well-formed but invalid, cannot parse data!
	
	# Constructor
	func _init():
		pass # intentionally do nothing
	
	# Enums
	# 3-bits HI
	enum MajorType {
		# integer types (fixed or variable length)
		UINT, # 0 = unsigned integer, range 0 to 2^64-1
		NINT, # 1 = negative integer, range -2^64 to -1
		# stream types (variable or indefinite length)
		BSTR, # 2 = byte string, length specified in info. i.e. raw-bytes
		TSTR, # 3 = text string, can contain unicode characters! i.e. multi-bytes
		ARR,  # 4 = array of data items. i.e. list
		MAP,  # 5 = map of pairs of data items. i.e. dictionary
		# special types
		TAG,  # 6 = optional semantic tagging as an integer id and content. i.e. extensions ...
		FLT,  # 7 = floating-point types, simple data types, or BREAK stop code (IETF ran out of bits?)
		# 7 is the maximum value that fits (7 == 0x07 == b00000111)
	}
	
	# note: these could be const masks but enums are more friendly for humans?
	# i.e. MajorType.TAG, AdditionalInfo.BREAK, etc
	
	# 5-bits LO
	enum AdditionalInfo {
		# less than 24 then this info IS the value or payload length
		B1 = 24, # 24 means value or length is in next byte
		B2,      # 25 means info in next 2 bytes
		B4,      # 26 means next 4 bytes
		B8,      # 27 next 8 bytes
		R28,     # 28-30 are reserved for future use, invalid format
		R29,     # invalid format
		R30,     # invalid format
		# 31 indicates indefinite length (major types 2-5 only) or terminator of that length (major type 7)
		BREAK,   # if major type is 0, 1, or 6 then this is an invalid format!
		# 31 is the maximum value that fits (31 == 0x1F == b00011111)
	}
	# Enums
	
	# Constants
	const non_stream_types = [MajorType.UINT, MajorType.NINT, MajorType.TAG, MajorType.FLT]
	const stream_types = [MajorType.BSTR, MajorType.TSTR, MajorType.ARR, MajorType.MAP]
	
	const byte_mask = 0xFF # 0b11111111
	const info_mask = 0x1F # 0b00011111
	const type_mask = 0xE0 # 0b11100000
	
	# half-float (16-bit)
	const f16_sign = 0x8000 # 1-bit
	const f16_expo = 0x7C00 # 5-bits
	const f16_mant = 0x03FF # 10-bits
	
	# float (32-bit)
	const f32_sign = 0x80000000 # 1-bit
	const f32_expo = 0x7F800000 # 8-bits
	const f32_mant = 0x007FFFFF # 23-bits
	
	# double (64-bit)
	# note: int64_t overflow, can just shift first?
	#const f64_sign = 0x8000000000000000 # 1-bit
	const f64_expo = 0x7FF0000000000000 # 11-bits
	const f64_mant = 0x000FFFFFFFFFFFFF # 52-bits
	
	# Simple values
	const FALSE = 20
	const TRUE = 21
	const NULL = 22
	const UNDEFINED = 23
	# Constants
	
	# Methods
	func fmt_error(what: int, why: String) -> void:
		emit_signal('invalid_format', what, why)
		print_debug('INVALID FORMAT: ', what, ' = ', why)
	
	func val_error(why: String) -> void:
		emit_signal('invalid_data', why)
		print_debug('INVALID DATA: ', why)
	
	func _is_litle_endian() -> bool:
		# todo: fixme: this is entirely implementation defined!
		# https://en.wikipedia.org/wiki/Endianness#Simplified_access_to_part_of_a_field
		# there is no concept of union in GDScript despite Godot being C++
		# all int vars are int64_t so const literal position may not matter
		# i.e. undefined behavior ... dragons may fly out your nose!
		return 0x4A == 0x0000004A
	# Methods

# Derived reader to help decode DataItem
class CBORReader extends CBORBase:
	# Signals
	signal end_of_data_items(idx, size)        # CBOR buffer exhausted, this may not be an error!
	
	# State
	var _raw_data: PoolByteArray
	var _cur_idx: int
	
	# Constructor
	func _init(copyBytes: PoolByteArray):
		_raw_data = copyBytes
		_cur_idx = 0
	
	# Methods
	# returns the decoded DataItem(s)
	func read_all():
		var items = []
		var item = read_next()
		while item != null:
			items.append(item)
			item = read_next()
		return items[0] if items.size() == 1 else items
	
	# returns DataItem (or null on failure, end of data, etc)
	func read_next():
		if _cur_idx < _raw_data.size():
			return read_item()
		emit_signal('end_of_data_items', _cur_idx, _raw_data.size())
		return null
	
	# returns MajorType
	func read_type(byte):
		return (byte & type_mask) >> 5
	
	# returns AdditionalInfo
	func read_info(byte):
		return (byte & info_mask)
	
	# returns next byte from buffer and increments index
	func read_byte():
		var byte
		if _cur_idx < _raw_data.size():
			byte = (_raw_data[_cur_idx] & byte_mask)
			_cur_idx += 1
		else:
			val_error('index of out range')
		return byte
	
	# read 1 bytes ...
	func read_u8():
		return read_byte()
	
	# read 2 bytes ...
	func read_u16():
		return (  (read_byte() << 8) 
				|  read_byte())
	
	# read 4 bytes ...
	func read_u32():
		return (  (read_byte() << 24) 
				| (read_byte() << 16)
				| (read_byte() << 8) 
				|  read_byte())
	
	# read 8 bytes ...
	func read_u64():
		return (  (read_byte() << 56) 
				| (read_byte() << 48)
				| (read_byte() << 40)
				| (read_byte() << 32)
				| (read_byte() << 24)
				| (read_byte() << 16)
				| (read_byte() << 8) 
				|  read_byte())
	
	# read header byte ...
	func read_header():
		var next_byte = read_byte()
		return { 
			'type': read_type(next_byte),
			'info': read_info(next_byte)
		}
	
	# read length bytes sub-slice ...
	func read_slice(length: int):
		var s: PoolByteArray
		var to = _cur_idx + length - 1 # inclusive index
		if to < _raw_data.size():
			s = _raw_data.subarray(_cur_idx, to)
			_cur_idx += length
		else:
			val_error('index out of range')
		return s
	
	# read DataItem length from header ...
	func read_length(header: Dictionary) -> int:
		match header.info:
			AdditionalInfo.B1:
				return 1 if header.type in non_stream_types else read_u8()
			AdditionalInfo.B2:
				return 2 if header.type in non_stream_types else read_u16()
			AdditionalInfo.B4:
				return 4 if header.type in non_stream_types else read_u32()
			AdditionalInfo.B8:
				return 8 if header.type in non_stream_types else read_u64()
			AdditionalInfo.R28:
				fmt_error(header.info, 'info reserved for future use')
				return -1
			AdditionalInfo.R29:
				fmt_error(header.info, 'info reserved for future use')
				return -1
			AdditionalInfo.R30:
				fmt_error(header.info, 'info reserved for future use')
				return -1
			AdditionalInfo.BREAK:
				match header.type:
					MajorType.UINT:
						fmt_error(header.type, 'type cannot have info BREAK (31)')
						return -1
					MajorType.NINT:
						fmt_error(header.type, 'type cannot have info BREAK (31)')
						return -1
					MajorType.TAG:
						fmt_error(header.type, 'type cannot have info BREAK (31)')
						return -1
					_:
						return 0 # indefinite length or terminator
			_:
				return header.info # stream short length, tag, etc
	
	# read DataItem payload from buffer ...
	func read_payload(header: Dictionary):
		var length = read_length(header)
		if length >= 0: # valid length?
			match header.type:
				MajorType.UINT:
					return read_integer(header, length, false)
				MajorType.NINT:
					return read_integer(header, length, true)
				MajorType.BSTR:
					return read_bstring(length)
				MajorType.TSTR: # unicode!
					return read_bstring(length).get_string_from_utf8()
				MajorType.ARR:
					return read_list(length)
				MajorType.MAP:
					return read_dictionary(length)
				MajorType.TAG:
					return read_tag(header, length)
				MajorType.FLT:
					if header.info < AdditionalInfo.B2:
						return read_simple(header)
					elif header.info < AdditionalInfo.R28:
						return read_float(length)
					elif header.info == AdditionalInfo.BREAK:
						return read_break()
					else:
						fmt_error(header.info, 'info not possible for major type 7')
				_:
					fmt_error(header.type, 'type format unknown or invalid')
		return null # invalid format
	
	# read DataItem from header and payload ...
	func read_item():
		return read_payload(read_header())
	
	# todo: implement all tag extensions!
	func read_tag(header: Dictionary, length: int):
		val_error("TAG not supported!")
		return null # todo: fixme: incorrect length?
		# todo: uri, mime, dates, basez, embedded CBOR in CBOR, etc
		match header.info: # tag number
			2: # bignum
				var tag_content = read_header()
				var item = read_payload(tag_content)
				# todo: reinterpret_cast as bignum?
				return item
			_:
				fmt_error(header.info, 'info is not tag number for major type 6')
		return null
	
	func read_simple(header: Dictionary):
		if header.info < AdditionalInfo.B1:
			match header.info:
				FALSE:
					return false
				TRUE:
					return true
				NULL:
					return 0 # todo: fixme: use a different sentinel value?
				UNDEFINED:
					return 'undefined' # todo: fixme: something other than string?
				_:
					return header.info # unassigned (0..19)
		elif header.info == AdditionalInfo.B1:
			var u = read_u8()
			if u < 32 or u > 255:
				val_error('SIMPLE value out of range for major type 7')
			else:
				return u # unassigned (32..255)
		else:
			fmt_error(header.info, 'info not parsed for major type 7')
		return null # invalid value
	
	func read_break():
		# 0xFF is stop code, we are done with an indefinite stream!
		return null
	
	func read_dictionary(length: int):
		var map: Dictionary = {}
		if length > 0:
			for i in length: # expect this many pairs without BREAK
				var next_key = read_item()
				var next_value = read_item()
				if next_key != null and next_value != null:
					map[next_key] = next_value
				else:
					val_error('invalid key/value pair')
					break
		else: # indefinite! we expect a BREAK eventually ...
			var next_key = read_item()
			var next_value = read_item()
			while next_key != null and next_value != null:
				map[next_key] = next_value
				next_key = read_item()
				if next_key != null: # break stop code?
					next_value = read_item()
		return map
	
	func read_list(length: int):
		var arr: Array = []
		if length > 0:
			arr.resize(length)
			for i in length: # expect this many items without BREAK
				var next_item = read_item()
				if next_item != null:
					arr[i] = next_item
				else:
					val_error('invalid data item')
					break
		else: # indefinite length!
			var next_item = read_item()
			while next_item != null: # keep going until BREAK
				arr.append(next_item)
				next_item = read_item()
		return arr
	
	func read_bstring(length: int):
		var byte_str: PoolByteArray
		if length > 0:
			byte_str = read_slice(length)
		else: # indefinite!
			byte_str = PoolByteArray()
			var byte = read_byte()
			while byte != null and byte != 0xFF:
				byte_str.append(byte)
		return byte_str
	
	func read_float(length: int):
		if not length in [2, 4, 8]:
			val_error('float byte length invalid for major type 7')
			return null
		# todo: fixme: nothing attempted has worked thus far ...
		var s = read_slice(length)
		# https://ideone.com/dSmFd9 ;-)
		val_error('cannot parse float: ' + s.hex_encode() + ' with size ' + str(length))
	
	func read_integer(header: Dictionary, length: int, negative: bool):
		var value: int
		if header.info < AdditionalInfo.B1: # info IS value, no payload
			value = header.info
		else:
			match length:
				1:
					value = read_u8()
				2:
					value = read_u16()
				4:
					value = read_u32()
				8:
					value = read_u64()
				_:
					val_error('integer byte length invalid for major type 0')
					return null
		if negative:
			value = -1 - value # CBOR is weird ...
		return value;
	# Methods

# todo: fixme: this is half baked and hardly working !!!
# Derived writer to help encode DataItem
class CBORWriter extends CBORBase:
	# Signals
	signal unsupported_data_type(type)         # CBOR does not support, this is not an "internet" type!
	
	# State
	var _raw_object # variant
	var _cbor: PoolByteArray
	
	# Constructor
	func _init(copyAnything):
		if copyAnything is Object and copyAnything.has_method('duplicate'):
			_raw_object = copyAnything.duplicate() # no references!
		else:
			_raw_object = copyAnything
		_cbor = PoolByteArray()
	
	func write_all():
		if _raw_object is Array:
			# todo: should we even flatten this?
			for obi in _raw_object:
				write_next()
		else:
			write_next() # just one DataItem
		return _cbor
	
	func write_next():
		if _raw_object is int:
			write_integer()
		elif _raw_object is float:
			write_float()
		elif _raw_object is String:
			write_string()
		elif _raw_object is Array:
			write_list()
		elif _raw_object is Dictionary:
			write_dictionary()
		elif _raw_object is bool or _raw_object == null:
			write_simple()
		# todo: add conditionals for TAG types here!
		else:
			emit_signal('unsupported_data_type', _raw_object)
	
	func flip_item(next):
		return next.invert() if _is_litle_endian() else next
	
	func write_header(type, length) -> int:
		var cnt: int
		var hdr = 0
		hdr |= ((type << 5) & type_mask)
		if length < AdditionalInfo.B1: # 0..23
			hdr |= (length & info_mask)
			cnt = 0 # payload value in header byte
		elif length < 0xFF: # uint8_t
			hdr |= (AdditionalInfo.B1 & info_mask)
			cnt = 1
		elif length < 0xFFFF: # uint16_t
			hdr |= (AdditionalInfo.B2 & info_mask)
			cnt = 2
		elif length < 0xFFFFFFFF: # uint32_t
			hdr |= (AdditionalInfo.B4 & info_mask)
			cnt = 4
		else: # uint64_t
			hdr |= (AdditionalInfo.B8 & info_mask)
			cnt = 8
		# todo: handle stream lengths!
		_cbor.append(hdr)
		return cnt # return byte count to avoid decoding header
	
	func write_integer():
		var val = _raw_object as int
		var neg = val < 0
		if neg:
			val = -val - 1
		var cnt = write_header(MajorType.NINT if neg else MajorType.UINT, val)
		while cnt > 0:
			cnt -= 1
			_cbor.append((val >> 8*cnt) & byte_mask) # always BE!

	func write_simple():
		# todo: implement me!
		pass
	
	func write_float():
		# todo: implement me!
		pass
		
	func write_string():
		# todo: implement me!
		pass
		
	func write_list():
		# todo: implement me!
		pass
		
	func write_dictionary():
		# todo: implement me!
		pass
		
	func write_tag():
		# todo: implement me!
		pass
########################################
######## Implementation Details ########
########################################
