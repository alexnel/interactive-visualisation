fdp -Tplain -O graphtest.dot
fdp -Tpng -O graphtest.dot
stack build
stack exec interactive-visualisation