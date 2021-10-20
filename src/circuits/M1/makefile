docs/M1_SystemCircuit.pdf :
	cd docs; pdflatex M1_System_Circuit.tex

.PHONY : clean
clean :
	find . \( -name '*~' \
	  -o -name '*.hi' \
	  -o -name '*.log' \
	  -o -name '*.aux' \
	  -o -name '*.out' \
	  -o -name '*.toc' \
	  -o -name '*.bak' \) -delete
	rm -rf docs/auto
	rm docs/M1_System_Circuit.tex

