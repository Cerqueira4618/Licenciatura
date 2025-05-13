jogo:
	gcc main.c -o jogo -Wall -Wextra -pedantic -O1 -fsanitize=address -fno-omit-frame-pointer -g

clean: 
	rm jogo
	rm res.txt
	
testar:
	gcc testes.c -o testes -lcunit -Wall -Wextra -pedantic -g
	./testes

tudo:
	make clean
	make jogo
	./jogo


#gcov programa-main
#open main.c.gcov - ve as linhas nao executadas

cobertura:
	gcc -fprofile-arcs -ftest-coverage -o programa main.c