.PHONY: clean

Lexers.hs: codegen.py _mapping.py
	python3 -B codegen.py > Lexers.hs

_mapping.py:
	# This mapping file is for pygments 2.6.1. You need to change the url to use in other versions
	wget https://raw.githubusercontent.com/pygments/pygments/d2eae8580d7e363bca03554e3b516d0fd77b25cb/pygments/lexers/_mapping.py

clean:
	rm -f Lexers.hs
	rm -f _mapping.py
