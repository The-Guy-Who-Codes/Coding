file = open("python_script.lua", "a")
file.write("lmc_device_set_name('737 Keyboard','1C8A767F')\nlmc_print_devices()\n\n\n")
file.write("lmc_set_handler((\'737 Keyboard\'), function(button,direction)\n\tif (direction == 1) then return end\n\tif (button == 65) then\n\t\tlmc_send_keys(\'a,{F9}\')\n\n\t\t")
keyboard_keys = {
    "BACKSPACE":8,
    "ENTER":13,
    "BREAK":19,
    "PGUP":33,
    "PGDN":34,
    "END":35,
    "HOME":36,
    "LEFT":37,
    "UP":38,
    "RIGHT":39,
    "DOWN":40,
    "INS":45,
    "DELETE":46,
    "0":48,
    "1":49,
    "2":50,
    "3":51,
    "4":52,
    "5":53,
    "6":54,
    "7":55,
    "8":56,
    "9":57,
   # "a":65,
    "b":66,
    "c":67,
    "d":69,
    "f":70,
    "g":71,
    "h":72,
    "i":73,
    "j":74,
    "k":75,
    "l":76,
    "m":77,
    "n":78,
    "o":79,
    "p":80,
    "q":81,
    "r":82,
    "s":83,
    "t":84,
    "u":85,
    "v":86,
    "w":87,
    "x":88,
    "y":89,
    "z":90,
    "NUM0":96,
    "NUM1":97,
    "NUM2":98,
    "NUM3":99,
    "NUM4":100,
    "NUM5":101,
    "NUM6":102,
    "NUM7":103,
    "NUM8":104,
    "NUM9":105,
    "NUMMULTIPLY":106,
    "NUMPLUS":107,
    "NUMMINUS":109,
    "NUMDECIMAL":110,
    "NUMDIVIDE":111,
    "F1":112,
    "F2":113,
    "F3":114,
    "F4":115,
    "F5":116,
    "F6":117,
    "F7":118,
    "F8":119,
    ";":186,
    "=":187,
    ",":188,
    "-":189,
    ".":190,
    "/":191,
    "`":192,
    "[":219,
    "\\\\":220,
    "]":221,
    "\\\'":222,
}

combining_key = "F9"

keys_list = list(keyboard_keys)
keys = list(keyboard_keys.values())

def code(iteration):
    if iteration < 13 or (iteration > 46 and iteration < 70):
        keycomb = "{" + keys_list[iteration] + "}{" + combining_key + "}"
    else:
        keycomb = keys_list[iteration] + "{" + combining_key +"}"
    code = "elseif (button == " + str(keys[iteration]) +")then\n\t\tlmc_send_keys(\'"+keycomb+"\')\n\n\t\t"
    return code

for x in range (len (keys)):
    file.write(code(x))
file.write("\n\tend\n\tend\n)")
file.close()