# Roller.gd
# Utility script for pseudo random number generation
class_name Roller
extends GDScript

static func roll(lo: int, hi: int):
	randomize()
	return randi() % (hi-lo+1) + lo

static func roll_d20():
	return roll(1, 20)

static func roll_combat(att, def):
	var r = roll_d20()
	if r == 1:
		return false
	elif r == 20:
		return true
	else:
		return att + r > def
