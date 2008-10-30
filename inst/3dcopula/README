This directory contains code for generation of 3D copula figures


The perl script shown by example in the next line

3dcopula.pl 3dcopula_config_example.txt 

drives R and LaTeX to create 3dcopula.pdf. The program creates a bunch of TMP*.txt files and creates a file called 3dcopula_fromperl.tex, which provides a sort of core communication mechansim from the program to LaTeX. The file 3dcopula_ancillary.tex (if exists) is read just before\begin{document} is executed in the 3dcopula.tex file.


3dcopula.pl --clean

cleans up temporary files and exits, nothing else is done



The examples directory contains additional example configuration files. Let us run a few simple ones:

3dcopula.pl 3dconfig_examples/circ.txt
3dcopula.pl 3dconfig_examples/PI.txt
3dcopula.pl --nsimcop=0  3dconfig_examples/M.txt
3dcopula.pl --nsimcop=20 3dconfig_examples/W.txt
