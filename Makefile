all:
	cd home && tar cvf - . | (cd /Users/larry; tar xvf -)