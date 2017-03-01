# interactive-visualisation

This program takes in a .dot file and displays it graphically. When you click on a node, the nodes name will be printed to screen.



# to run program the first time: 

chmod +x ./runIV.sh
./run.sh

# to run the program after that in stack:

./run.sh format filename


where 
format is the graphviz dot format you would like to depict.
        e.g. dot, neato, sfdp, fdo, twopi, circo
      
filename is the .dot file you would like to work with.
        e.g. test.dot




# dependencies:  
graphviz,
	           text,
	           fgl,
	           filepath,
	           gloss-juicy,
	           GLFW-b,
	           OpenGL,
	           bytestring,
	           gloss,
	           gloss-rendering,
	           GLUT,
	           split,
	           mtl,
	           containers





# Please take note:
This program currently only works for small graphs. I am working to try and fix this.