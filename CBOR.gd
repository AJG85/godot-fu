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
const semver = "1.1.1" # bump me accordingly https://semver.org
###############################################################################
# - VERSION - | - AUTHOR - | -------------- SUMMARY OF CHANGES -------------- |
#    1.0.0    |  DinoDude  |  Initial implementation of encode, decode, etc   |
#    1.1.0    |  DinoDude  |  Additional encoders and methods via agent       |
#    1.1.1    |  DinoDude  |  Minor fixes, updates, and testing               |
###############################################################################
#--------------------------------------- START API
# HEY YOU! Do you like computer science even more than video games?
# If so, please consider making a pull request to improve this for posterity.
# You can report issues and bugs but it is better to fix errors yourself!
# GitHub: https://www.github.com/AJG85/godot-fu
#--------------------------------------- TESTING
static func test():
	# For this example usage test simply add to any node:
	# func _ready():
	# 	CBOR.test() # run and print test results
	# note: increase max chars per second for stdout in project settings temporarily!

	# todo: add more test cases or stream JSON vector?
	# https://github.com/cbor/test-vectors/blob/master/appendix_a.json
	print('=== CBOR.gd (version ', semver, ') === Test Mode ===')
	print()

	var test_cnt = 1
	print('Test #', test_cnt, ': b16 postive integer I/O ...')
	var test_in = 42
	var res_in = hex_encode(encode(test_in))
	print(res_in) # 182a
	var test_out = res_in
	var res_out = decode(hex_decode(test_out))
	print(res_out) # 42
	print('Test #', test_cnt, ': ', 'PASS' if test_in == res_out else 'FAIL')
	print()

	test_cnt += 1
	print('Test #', test_cnt, ': b16 negative integer I/O ...')
	test_in = -123456789
	res_in = hex_encode(encode(test_in))
	print(res_in) # 3a075bcd14
	test_out = res_in
	res_out = decode(hex_decode(test_out))
	print(res_out) # -123456789
	print('Test #', test_cnt, ': ', 'PASS' if test_in == res_out else 'FAIL')
	print()

	test_cnt += 1
	print('Test #', test_cnt, ': b16 big integer I/O ...')
	test_in = 1_000_000_000_000
	res_in = hex_encode(encode(test_in))
	print(res_in) # 1b000000e8d4a51000
	test_out = res_in
	res_out = decode(hex_decode(test_out))
	print(res_out) # 1000000000000
	print('Test #', test_cnt, ': ', 'PASS' if test_in == res_out else 'FAIL')
	print()

	test_cnt += 1
	print('Test #', test_cnt, ': b16 object I/O ...')
	test_in = {'hello':'world'}
	res_in = hex_encode(encode(test_in))
	print(res_in) # a16568656c6c6f65776f726c64
	test_out = res_in
	res_out = decode(hex_decode(test_out))
	print(res_out) # {hello:world}
	print('Test #', test_cnt, ': ', 'PASS' if test_in.hash() == res_out.hash() else 'FAIL')
	print()

	test_cnt += 1
	print('Test #', test_cnt, ': b16 double I/O ...')
	test_in = 3.14159
	res_in = hex_encode(encode(test_in))
	print(res_in) # fb400921f9f01b866e
	test_out = res_in
	res_out = decode(hex_decode(test_out))
	print(res_out) # 3.14159
	print('formatted: %f' % res_out) # 3.141590
	print('Test #', test_cnt, ': ', 'PASS' if test_in == res_out else 'FAIL')
	print()

	test_cnt += 1
	print('Test #', test_cnt, ': b16 float I/O ...')
	test_out = 'fa40490fdb'
	print(test_out) # fa40490fdb
	res_out = decode(hex_decode(test_out))
	print(res_out) # 3.141593
	print('formatted: %.5f' % res_out) # 3.14159
	print('Test #', test_cnt, ': ', 'PASS' if abs(res_out - test_in) < 1e-5  else 'FAIL')
	print()

	test_cnt += 1
	print('Test #', test_cnt, ': b16 half-float I/O ...')
	test_out = 'f94248'
	print(test_out) # f94248
	res_out = decode(hex_decode(test_out))
	print(res_out) # 3.140625
	print('formatted: %.2f' % res_out) # 3.14
	print('Test #', test_cnt, ': ', 'PASS' if abs(res_out - test_in) < 5e-3 else 'FAIL')
	print()

	test_cnt += 1
	print('Test #', test_cnt, ': b64 array I/O ...')
	test_in = [1, 2]
	res_in = Marshalls.raw_to_base64(encode(test_in))
	print(res_in) # ggEC
	test_out = res_in
	res_out = decode(Marshalls.base64_to_raw(test_out))
	print(res_out) # [1, 2]
	print('Test #', test_cnt, ': ', 'PASS' if test_in == res_out else 'FAIL')
	print()

	test_cnt += 1
	print('Test #', test_cnt, ': b64 nested arrays I/O ...')
	test_in = [[1, 2], [3, 4], [5, 6]]
	res_in = Marshalls.raw_to_base64(encode(test_in))
	print(res_in) # g4IBAoIDBIIFBg==
	test_out = res_in
	res_out = decode(Marshalls.base64_to_raw(test_out))
	print(res_out) # [[1, 2], [3, 4], [5, 6]]
	print('Test #', test_cnt, ': ', 'PASS' if test_in == res_out else 'FAIL')
	print()

	test_cnt += 1
	print('Test #', test_cnt, ': raw multi-type object I/O ...')
	test_in = {'compact': true, 'schema': 0}
	res_in = encode(test_in)
	print(res_in) # [162, 103, 99, 111, 109, 112, 97, 99, 116, 245, 102, 115, 99, 104, 101, 109, 97, 0]
	test_out = res_in
	res_out = decode(test_out)
	print(res_out) # {compact:True, schema:0}
	print('Test #', test_cnt, ': ', 'PASS' if test_in.hash() == res_out.hash() else 'FAIL')
	print()

	test_cnt += 1
	print('Test #', test_cnt, ': b16 array of nested multi-type objects I/O ...')
	test_in = [{'a': false, 'b': 'c'}, {'d': 1, 'e': {'f': -1} }]
	res_in = hex_encode(encode(test_in))
	print(res_in) # 82a26161f461626163a26164016165a1616620
	test_out = res_in
	res_out = decode(hex_decode(test_out))
	print(res_out) # [{a:False, b:c}, {d:1, e:{f:-1}}]
	print('Test #', test_cnt, ': ', 'PASS' if test_in.hash() == res_out.hash() else 'FAIL')
	print()

	# --- unsigned integer boundary decodes (appendix A) ---
	print('--- unsigned integer boundaries ---')
	var uint_vecs = [['00',0],['01',1],['0a',10],['17',23],['1818',24],['18ff',255],['190100',256],['1903e8',1000]]
	for v in uint_vecs:
		test_cnt += 1
		_chk(test_cnt, 'uint ' + v[0], _dc(v[0]), v[1])
	print()

	# --- negative integer boundary decodes (appendix A) ---
	print('--- negative integer boundaries ---')
	var nint_vecs = [['20',-1],['29',-10],['3863',-100],['3903e7',-1000]]
	for v in nint_vecs:
		test_cnt += 1
		_chk(test_cnt, 'nint ' + v[0], _dc(v[0]), v[1])
	print()

	# --- simple values ---
	print('--- simple values ---')
	test_cnt += 1; _chk(test_cnt, 'false decode', _dc('f4'), false)
	test_cnt += 1; _chk(test_cnt, 'true decode', _dc('f5'), true)
	test_cnt += 1; _chk(test_cnt, 'null decode', _dc('f6'), null)
	test_cnt += 1; _chk(test_cnt, 'false roundtrip', _rt(false), false)
	test_cnt += 1; _chk(test_cnt, 'true roundtrip', _rt(true), true)
	test_cnt += 1; _chk(test_cnt, 'null roundtrip', _rt(null), null)
	print()

	# --- text strings ---
	print('--- text strings ---')
	test_cnt += 1; _chk(test_cnt, 'empty string', _dc('60'), '')
	test_cnt += 1; _chk(test_cnt, 'string a', _dc('6161'), 'a')
	test_cnt += 1; _chk(test_cnt, 'string IETF', _dc('6449455446'), 'IETF')
	test_cnt += 1; _chk(test_cnt, 'string roundtrip', _rt('hello'), 'hello')
	print()

	# --- byte strings (decode only, no PoolByteArray encoder) ---
	print('--- byte strings ---')
	test_cnt += 1; _chk(test_cnt, 'empty bstr', _dc('40'), PoolByteArray())
	test_cnt += 1; _chk(test_cnt, 'bstr [1,2,3,4]', _dc('4401020304'), PoolByteArray([1, 2, 3, 4]))
	print()

	# --- empty containers ---
	print('--- empty containers ---')
	test_cnt += 1; _chk(test_cnt, 'empty array decode', _dc('80'), [])
	test_cnt += 1; _chk(test_cnt, 'empty map decode', _dc('a0'), {})
	test_cnt += 1; _chk(test_cnt, 'empty array roundtrip', _rt([]), [])
	test_cnt += 1; _chk(test_cnt, 'empty map roundtrip', _rt({}), {})
	print()

	# --- null in containers (validates CBORSentinel does not swallow null items) ---
	print('--- null in containers ---')
	test_cnt += 1; _chk(test_cnt, 'array with null', _rt([1, null, 2]), [1, null, 2])
	print()

	# --- indefinite length arrays (appendix A) ---
	print('--- indefinite length ---')
	test_cnt += 1; _chk(test_cnt, 'indef empty array', _dc('9fff'), [])
	test_cnt += 1; _chk(test_cnt, 'indef array [1,2]', _dc('9f0102ff'), [1, 2])
	test_cnt += 1; _chk(test_cnt, 'indef nested [1,[2,3],[4,5]]', _dc('9f018202039f0405ffff'), [1, [2, 3], [4, 5]])
	print()

	# --- B1 length encoding (>23 items, exercises read_u8 length path, decode only) ---
	print('--- B1 length encoding ---')
	var arr25 = []
	for i in 25: arr25.append(i + 1)
	test_cnt += 1; _chk(test_cnt, 'array of 25 items', _dc('98190102030405060708090a0b0c0d0e0f101112131415161718181819'), arr25)
	print()

#--------------------------------------- TEST HELPERS (private)
static func _dc(hex: String):
	return decode(hex_decode(hex))

static func _rt(val):
	return decode(encode(val))

static func _chk(cnt: int, label: String, got, want) -> void:
	var ok: bool
	if got is Dictionary and want is Dictionary:
		ok = got.hash() == want.hash()
	else:
		ok = got == want
	print('Test #', cnt, ': ', label, ' ... ', 'PASS' if ok else 'FAIL')

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

class CBORSentinel:
	const INT_MAX := 9223372036854775807
	enum Reason { EOD, BREAK, ERROR }
	var reason
	func _init(r = Reason.EOD):
		reason = r

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
	# Methods

# Derived reader to help decode DataItem
class CBORReader extends CBORBase:
	# Signals
	signal end_of_data_items(idx, size)        # CBOR buffer exhausted, this may not be an error!

	func eod(idx: int, size: int) -> void:
		emit_signal('end_of_data_items', idx, size)
		if (idx != size):
			print_debug('EOD: ', idx, ' of ', size, ' bytes read')

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
		while not item is CBORSentinel:
			items.append(item)
			item = read_next()
		return items[0] if items.size() == 1 else items

	# returns DataItem (or CBORSentinel on failure, end of data, etc)
	func read_next():
		if _cur_idx < _raw_data.size():
			return read_item()
		eod(_cur_idx, _raw_data.size())
		return CBORSentinel.new(CBORSentinel.Reason.EOD)

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
			val_error('index out of range')
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
						return CBORSentinel.INT_MAX # indefinite length or terminator
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
		return CBORSentinel.new(CBORSentinel.Reason.ERROR) # invalid format

	# read DataItem from header and payload ...
	func read_item():
		return read_payload(read_header())

	# todo: implement semantic tag extensions with refactor extract?
# warning-ignore:unused_argument
	func read_tag(header: Dictionary, length: int):
		return CBORSentinel.new(CBORSentinel.Reason.ERROR)
		# todo: uri, mime, dates, basez, embedded CBOR in CBOR, etc
# warning-ignore:unreachable_code
		match header.info: # tag number
			2: # bignum
				var tag_content = read_header()
				var item = read_payload(tag_content)
				# todo: reinterpret_cast as bignum?
				return item
			18: # signature
				var tag_content = read_header()
				var item = read_payload(tag_content)
				# todo: COSE_Sign1
				return item
			32: # uri
				var tag_content = read_header()
				var item = read_payload(tag_content)
				# todo: decode URI component
				return item
			258: # set
				var tag_content = read_header()
				var item = read_payload(tag_content)
				# todo: decode set of ledger transactions
				return item
			_:
				fmt_error(header.info, 'info is not tag number for major type 6')
		return CBORSentinel.new(CBORSentinel.Reason.ERROR)

	func read_simple(header: Dictionary):
		if header.info < AdditionalInfo.B1:
			match header.info:
				FALSE:
					return false
				TRUE:
					return true
				NULL:
					return null
				UNDEFINED:
					return null # todo: consider introducing distinct type?
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
		return CBORSentinel.new(CBORSentinel.Reason.ERROR)

	func read_break():
		# 0xFF is stop code, we are done with an indefinite stream!
		return CBORSentinel.new(CBORSentinel.Reason.BREAK)

	func read_dictionary(length: int):
		var map: Dictionary = {}
		# indefinite! we expect a BREAK eventually ...
		if length == CBORSentinel.INT_MAX:
			var next_key = read_item()
			var next_value = read_item()
			while not next_key is CBORSentinel and not next_value is CBORSentinel:
				map[next_key] = next_value
				next_key = read_item()
				# this check is for BREAK stop code (do not remove!)
				if not next_key is CBORSentinel:
					next_value = read_item()
		elif length > 0:
			for i in length: # expect this many pairs without BREAK
				var next_key = read_item()
				var next_value = read_item()
				if not next_key is CBORSentinel and not next_value is CBORSentinel:
					map[next_key] = next_value
				else:
					val_error('invalid key/value pair')
					return CBORSentinel.new(CBORSentinel.Reason.ERROR)
		return map

	func read_list(length: int):
		var arr: Array = []
		# indefinite! we expect a BREAK eventually ...
		if length == CBORSentinel.INT_MAX:
			var next_item = read_item()
			while not next_item is CBORSentinel: # keep going until BREAK
				arr.append(next_item)
				next_item = read_item()
		elif length > 0:
			arr.resize(length)
			for i in length: # expect this many items without BREAK
				var next_item = read_item()
				if not next_item is CBORSentinel:
					arr[i] = next_item
				else:
					val_error('invalid data item')
					return CBORSentinel.new(CBORSentinel.Reason.ERROR)
		return arr

	func read_bstring(length: int):
		var byte_str: PoolByteArray
		# indefinite! we expect a BREAK eventually ...
		if length == CBORSentinel.INT_MAX:
			byte_str = PoolByteArray()
			var byte = read_byte()
			# fixme: unclear sentinel values (!EOD && !BREAK)
			while byte != null and byte != 0xFF:
				byte_str.append(byte)
		elif length > 0:
			byte_str = read_slice(length)
		return byte_str

	func read_float(length: int):
		if not length in [2, 4, 8]:
			val_error('float byte length invalid for major type 7')
			return CBORSentinel.new(CBORSentinel.Reason.ERROR)
		var s = read_slice(length)
		if s.size() != length:
			val_error('could not read not read all required bytes')
			return CBORSentinel.new(CBORSentinel.Reason.ERROR)
		var spb = StreamPeerBuffer.new()
		spb.big_endian = true
		spb.put_data(s)
		spb.seek(0)
		match length:
			2: return _decode_half_float(spb.get_data_array())
			4: return spb.get_float()
			8: return spb.get_double()
		return CBORSentinel.new(CBORSentinel.Reason.ERROR)

	# half-float (16-bit) IEEE 754 decode using f16_* constants from CBORBase
	func _decode_half_float(bytes: PoolByteArray) -> float:
		var bits = (bytes[0] << 8) | bytes[1]
		var sign_bit = -1.0 if (bits & f16_sign) != 0 else 1.0
		var exp_bits = (bits & f16_expo) >> 10
		var mant_bits = float(bits & f16_mant)
		if exp_bits == 0:
			return sign_bit * pow(2.0, -14.0) * (mant_bits / 1024.0)
		elif exp_bits == 31:
			return sign_bit * INF if mant_bits == 0.0 else NAN
		return sign_bit * pow(2.0, float(exp_bits - 15)) * (1.0 + mant_bits / 1024.0)

	func read_integer(header: Dictionary, length: int, negative: bool):
		var value: int
		if header.info < AdditionalInfo.B1: # info IS value, no payload
			value = header.info
		else:
			match length:
				1: value = read_u8()
				2: value = read_u16()
				4: value = read_u32()
				8: value = read_u64()
				_:
					val_error('integer byte length invalid for major type 0')
					return CBORSentinel.new(CBORSentinel.Reason.ERROR)
		if negative:
			value = -1 - value # CBOR is weird ...
		return value;
	# Methods

# Derived writer to help encode DataItem
class CBORWriter extends CBORBase:
	# Signals
	signal unsupported_data_type(obj) # CBOR does not support, this is not an "internet" type!

	func unsupported(obj) -> void:
		emit_signal('unsupported_data_type', obj)
		print_debug('UNSUPPORTED: %v' % obj)

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

	# alias for public API consistency
	func write_all():
		write_next() # always just one DataItem
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
			unsupported(_raw_object)

	func write_int_header(type, val) -> int:
		var cnt = -1
		var hdr = 0
		if not val is int:
			val_error('writing integer header requires integer value')
			return cnt
		hdr |= ((type << 5) & type_mask)
		if val < AdditionalInfo.B1: # 0..23
			hdr |= (val & info_mask)
			cnt = 0 # payload value in header byte
		elif val < 0xFF: # uint8_t
			hdr |= (AdditionalInfo.B1 & info_mask)
			cnt = 1
		elif val < 0xFFFF: # uint16_t
			hdr |= (AdditionalInfo.B2 & info_mask)
			cnt = 2
		elif val < 0xFFFFFFFF: # uint32_t
			hdr |= (AdditionalInfo.B4 & info_mask)
			cnt = 4
		else: # uint64_t (assumed)
			hdr |= (AdditionalInfo.B8 & info_mask)
			cnt = 8
		_cbor.append(hdr)
		return cnt # return byte count to avoid decoding header

	func write_integer():
		var val = _raw_object as int
		var neg = val < 0
		if neg:
			val = -val - 1
		# header [type|size_from_val]
		var cnt = write_int_header(MajorType.NINT if neg else MajorType.UINT, val)
		while cnt > 0:
			cnt -= 1
			_cbor.append((val >> 8*cnt) & byte_mask) # always BE!

	func write_simple():
		# header [type|val]
		var type_byte = (MajorType.FLT << 5) # SIMPLE reuses float type
		if _raw_object is bool:
			_cbor.append(type_byte | (TRUE if _raw_object else FALSE))
		elif _raw_object == null:
			_cbor.append(type_byte | NULL)
		else:
			_cbor.append(type_byte | UNDEFINED)

	func write_float():
		var val = _raw_object as float
		# header [type|size]
		_cbor.append((MajorType.FLT << 5) | AdditionalInfo.B8) # always 64-bit double
		var spb = StreamPeerBuffer.new()
		spb.big_endian = true
		spb.put_double(val)
		_cbor.append_array(spb.get_data_array())

	func write_string():
		var utf8 = (_raw_object as String).to_utf8()
		_cbor.append((MajorType.TSTR << 5) | utf8.size())
		_cbor.append_array(utf8)

	func write_list():
		var arr = _raw_object as Array
		# header [type|size]
		_cbor.append((MajorType.ARR << 5) | arr.size())
		for item in arr:
			var sub = CBORWriter.new(item).write_all()
			if sub != null:
				_cbor.append_array(sub)
			else:
				val_error('unwritable sub item dropped')

	func write_dictionary():
		var dict = _raw_object as Dictionary
		# header [type|size]
		_cbor.append((MajorType.MAP << 5) | dict.size())
		for key in dict:
			var kw = CBORWriter.new(key).write_all()
			var vw = CBORWriter.new(dict[key]).write_all()
			if (kw != null and vw != null):
				_cbor.append_array(kw)
				_cbor.append_array(vw)
			else:
				val_error('unwritable key/value pair dropped')

	func write_tag():
		# todo: implement me!
		pass
########################################
######## Implementation Details ########
########################################
