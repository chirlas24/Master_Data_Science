def rm_letter(string, index):
	if not string:
		print('Warning, not string')
	else:
		return string[:index-1] + string[index:]

def wc(string):
	#Calculamos n_word
	n_words = len(string.split(" "))
	#Calculamos n_lines
	n_lines = len(string.split("\n"))
	#Calculamos length
	length = len(string)
	#Devolvemos resultado
	return ("words: %i, lines: %i, length: %i" % (n_words, n_lines, length))

def centenario(name, year):
	if not isinstance(year, int):
		year = int(year)
	ann = year + 100
	return('%s cumplira 100 años en el año %i' % (name, ann))
     


