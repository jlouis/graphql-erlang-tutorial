default: dht

graphql:
	lualatex graphql.tex

dht: module_layout.pdf
	lualatex dht.tex

module_layout.pdf: module_layout.dot
	dot -Tpdf module_layout.dot > module_layout.pdf

