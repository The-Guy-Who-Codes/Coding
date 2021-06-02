lmc_device_set_name('737 Keyboard','1C8A767F')
lmc_print_devices()


lmc_set_handler(('737 Keyboard'), function(button,direction)
	if (direction == 1) then return end
	if (button == 65) then
		lmc_send_keys('a,{F9}')

		elseif (button == 8)then
		lmc_send_keys('{BACKSPACE}{F9}')

		elseif (button == 13)then
		lmc_send_keys('{ENTER}{F9}')

		elseif (button == 19)then
		lmc_send_keys('{BREAK}{F9}')

		elseif (button == 33)then
		lmc_send_keys('{PGUP}{F9}')

		elseif (button == 34)then
		lmc_send_keys('{PGDN}{F9}')

		elseif (button == 35)then
		lmc_send_keys('{END}{F9}')

		elseif (button == 36)then
		lmc_send_keys('{HOME}{F9}')

		elseif (button == 37)then
		lmc_send_keys('{LEFT}{F9}')

		elseif (button == 38)then
		lmc_send_keys('{UP}{F9}')

		elseif (button == 39)then
		lmc_send_keys('{RIGHT}{F9}')

		elseif (button == 40)then
		lmc_send_keys('{DOWN}{F9}')

		elseif (button == 45)then
		lmc_send_keys('{INS}{F9}')

		elseif (button == 46)then
		lmc_send_keys('{DELETE}{F9}')

		elseif (button == 48)then
		lmc_send_keys('0{F9}')

		elseif (button == 49)then
		lmc_send_keys('1{F9}')

		elseif (button == 50)then
		lmc_send_keys('2{F9}')

		elseif (button == 51)then
		lmc_send_keys('3{F9}')

		elseif (button == 52)then
		lmc_send_keys('4{F9}')

		elseif (button == 53)then
		lmc_send_keys('5{F9}')

		elseif (button == 54)then
		lmc_send_keys('6{F9}')

		elseif (button == 55)then
		lmc_send_keys('7{F9}')

		elseif (button == 56)then
		lmc_send_keys('8{F9}')

		elseif (button == 57)then
		lmc_send_keys('9{F9}')

		elseif (button == 66)then
		lmc_send_keys('b{F9}')

		elseif (button == 67)then
		lmc_send_keys('c{F9}')

		elseif (button == 69)then
		lmc_send_keys('d{F9}')

		elseif (button == 70)then
		lmc_send_keys('f{F9}')

		elseif (button == 71)then
		lmc_send_keys('g{F9}')

		elseif (button == 72)then
		lmc_send_keys('h{F9}')

		elseif (button == 73)then
		lmc_send_keys('i{F9}')

		elseif (button == 74)then
		lmc_send_keys('j{F9}')

		elseif (button == 75)then
		lmc_send_keys('k{F9}')

		elseif (button == 76)then
		lmc_send_keys('l{F9}')

		elseif (button == 77)then
		lmc_send_keys('m{F9}')

		elseif (button == 78)then
		lmc_send_keys('n{F9}')

		elseif (button == 79)then
		lmc_send_keys('o{F9}')

		elseif (button == 80)then
		lmc_send_keys('p{F9}')

		elseif (button == 81)then
		lmc_send_keys('q{F9}')

		elseif (button == 82)then
		lmc_send_keys('r{F9}')

		elseif (button == 83)then
		lmc_send_keys('s{F9}')

		elseif (button == 84)then
		lmc_send_keys('t{F9}')

		elseif (button == 85)then
		lmc_send_keys('u{F9}')

		elseif (button == 86)then
		lmc_send_keys('v{F9}')

		elseif (button == 87)then
		lmc_send_keys('w{F9}')

		elseif (button == 88)then
		lmc_send_keys('x{F9}')

		elseif (button == 89)then
		lmc_send_keys('y{F9}')

		elseif (button == 90)then
		lmc_send_keys('z{F9}')

		elseif (button == 96)then
		lmc_send_keys('{NUM0}{F9}')

		elseif (button == 97)then
		lmc_send_keys('{NUM1}{F9}')

		elseif (button == 98)then
		lmc_send_keys('{NUM2}{F9}')

		elseif (button == 99)then
		lmc_send_keys('{NUM3}{F9}')

		elseif (button == 100)then
		lmc_send_keys('{NUM4}{F9}')

		elseif (button == 101)then
		lmc_send_keys('{NUM5}{F9}')

		elseif (button == 102)then
		lmc_send_keys('{NUM6}{F9}')

		elseif (button == 103)then
		lmc_send_keys('{NUM7}{F9}')

		elseif (button == 104)then
		lmc_send_keys('{NUM8}{F9}')

		elseif (button == 105)then
		lmc_send_keys('{NUM9}{F9}')

		elseif (button == 106)then
		lmc_send_keys('{NUMMULTIPLY}{F9}')

		elseif (button == 107)then
		lmc_send_keys('{NUMPLUS}{F9}')

		elseif (button == 109)then
		lmc_send_keys('{NUMMINUS}{F9}')

		elseif (button == 110)then
		lmc_send_keys('{NUMDECIMAL}{F9}')

		elseif (button == 111)then
		lmc_send_keys('{NUMDIVIDE}{F9}')

		elseif (button == 112)then
		lmc_send_keys('{F1}{F9}')

		elseif (button == 113)then
		lmc_send_keys('{F2}{F9}')

		elseif (button == 114)then
		lmc_send_keys('{F3}{F9}')

		elseif (button == 115)then
		lmc_send_keys('{F4}{F9}')

		elseif (button == 116)then
		lmc_send_keys('{F5}{F9}')

		elseif (button == 117)then
		lmc_send_keys('{F6}{F9}')

		elseif (button == 118)then
		lmc_send_keys('{F7}{F9}')

		elseif (button == 119)then
		lmc_send_keys('{F8}{F9}')

		elseif (button == 186)then
		lmc_send_keys(';{F9}')

		elseif (button == 187)then
		lmc_send_keys('={F9}')

		elseif (button == 188)then
		lmc_send_keys(',{F9}')

		elseif (button == 189)then
		lmc_send_keys('-{F9}')

		elseif (button == 190)then
		lmc_send_keys('.{F9}')

		elseif (button == 191)then
		lmc_send_keys('/{F9}')

		elseif (button == 192)then
		lmc_send_keys('`{F9}')

		elseif (button == 219)then
		lmc_send_keys('[{F9}')

		elseif (button == 220)then
		lmc_send_keys('\\{F9}')

		elseif (button == 221)then
		lmc_send_keys(']{F9}')

		elseif (button == 222)then
		lmc_send_keys('\'{F9}')

		
	end
	end
)