default: graphql.pdf

graphql.pdf: graphql.tex compiler.pdf
	lualatex graphql.tex

compiler.pdf: compiler.dot
	dot -Tpdf compiler.dot > compiler.pdf

