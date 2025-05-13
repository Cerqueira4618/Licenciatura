#include "funcoes.c"
#include <CUnit/CUnit.h>
#include <CUnit/Basic.h>
#include <string.h>
#include <ctype.h>

void test_determinarCoordenadas() {
    int x, y;
    CU_ASSERT_TRUE(determinarCoordenadas("a1", &x, &y));
    CU_ASSERT_EQUAL(x, 0);
    CU_ASSERT_EQUAL(y, 0);

    CU_ASSERT_TRUE(determinarCoordenadas("c3", &x, &y));
    CU_ASSERT_EQUAL(x, 2);
    CU_ASSERT_EQUAL(y, 2);

    CU_ASSERT_FALSE(determinarCoordenadas("1a", &x, &y));
    CU_ASSERT_FALSE(determinarCoordenadas("a0", &x, &y));
    CU_ASSERT_FALSE(determinarCoordenadas("", &x, &y));
}

void test_salvar_e_carregar_jogo() {
    int L = 2, C = 3;
    char **matriz = malloc(L * sizeof(char *));
    matriz[0] = strdup("abc");
    matriz[1] = strdup("def");
    
    // Inicializar o histórico para o teste
    HistoricoPilha *hp = inicializarHistorico();

    salvarJogo(hp, "teste_jogo.txt", L, C, matriz);

    char **carregado = NULL;
    int l2, c2;

    CU_ASSERT_TRUE(carregarJogo("teste_jogo.txt", &l2, &c2, &carregado));
    CU_ASSERT_EQUAL(l2, L);
    CU_ASSERT_EQUAL(c2, C);
    CU_ASSERT_STRING_EQUAL(carregado[0], "abc");
    CU_ASSERT_STRING_EQUAL(carregado[1], "def");

    // Liberar memória
    free(matriz[0]); free(matriz[1]); free(matriz);
    free(carregado[0]); free(carregado[1]); free(carregado);
    liberarHistorico(hp);
    remove("teste_jogo.txt");
}

void test_executarComandoR_B() {
    int L = 2, C = 3;
    char **matriz = malloc(L * sizeof(char *));
    matriz[0] = strdup("abc");
    matriz[1] = strdup("def");
    
    // Inicializar o histórico para o teste
    HistoricoPilha *hp = inicializarHistorico();

    executarComandoR(hp, L, C, matriz, "r a2");
    CU_ASSERT_EQUAL(matriz[0][1], '#');

    executarComandoB(hp, L, C, matriz, "b b3");
    CU_ASSERT_EQUAL(matriz[1][2], 'F'); // 'f' vira 'F'

    liberarHistorico(hp);
    free(matriz[0]); free(matriz[1]); free(matriz);
}

void test_printMatriz() {
    // Apenas verificar se a função roda sem erros
    char **matriz = malloc(2 * sizeof(char *));
    matriz[0] = strdup("ab");
    matriz[1] = strdup("cd");

    printMatriz(2, 2, matriz);

    free(matriz[0]); free(matriz[1]); free(matriz);
}

void test_empilhar_e_desfazer_comando() {
    int L = 2, C = 3;
    char **matriz = malloc(L * sizeof(char *));
    matriz[0] = strdup("abc");
    matriz[1] = strdup("def");
    
    // Inicializar o histórico para o teste
    HistoricoPilha *hp = inicializarHistorico();

    // Empilhar o estado inicial
    empilharComando(hp, 'r', L, C, matriz);

    // Modificar a matriz
    matriz[0][0] = '#';

    // Desfazer o comando
    desfazerComando(hp, &matriz, &L, &C);

    // Verificar se o estado foi restaurado
    CU_ASSERT_STRING_EQUAL(matriz[0], "abc");
    CU_ASSERT_STRING_EQUAL(matriz[1], "def");

    liberarHistorico(hp);
    free(matriz[0]); free(matriz[1]); free(matriz);
}

void test_resolverJogo() {
    int L = 5, C = 5;
    char **matriz = malloc(L * sizeof(char *));
    matriz[0] = strdup("ecadc");
    matriz[1] = strdup("dcdec");
    matriz[2] = strdup("bddce");
    matriz[3] = strdup("cdeeb");
    matriz[4] = strdup("accbb");
    
    // Inicializar o histórico para o teste
    HistoricoPilha *hp = inicializarHistorico();

    // Primeiro vamos salvar a matriz no arquivo res.txt
    salvarJogoSem("res.txt", L, C, matriz);
    
    // Executar a resolução do jogo
    resolverJogo(hp, L, C, matriz);

    // Agora vamos carregar o resultado de res.txt para verificar
    char **resultado = NULL;
    int L2, C2;
    CU_ASSERT_TRUE(carregarJogo("res.txt", &L2, &C2, &resultado));
    
    // Verificar o estado final esperado
    CU_ASSERT_STRING_EQUAL(resultado[0], "E#ADC");
    CU_ASSERT_STRING_EQUAL(resultado[1], "DC#E#");
    CU_ASSERT_STRING_EQUAL(resultado[2], "B#DCE");
    CU_ASSERT_STRING_EQUAL(resultado[3], "CDE#B");
    CU_ASSERT_STRING_EQUAL(resultado[4], "A#CB#");

    // Limpar memória
    liberarHistorico(hp);
    for (int i = 0; i < L; i++) {
        free(matriz[i]);
    }
    free(matriz);
    
    for (int i = 0; i < L2; i++) {
        free(resultado[i]);
    }
    free(resultado);
}

void test_executarComandoa_caso1() {
    int L = 4, C = 4;
    char **matriz = malloc(L * sizeof(char *));
    matriz[0] = strdup("abcd");
    matriz[1] = strdup("a#cd");
    matriz[2] = strdup("abcd");
    matriz[3] = strdup("abcd");
    
    // Inicializar o histórico para o teste
    HistoricoPilha *hp = inicializarHistorico();

    executarComandoa(hp, L, C, matriz);

    // Verificar o estado final esperado
    CU_ASSERT_STRING_EQUAL(matriz[0], "aBcd");
    CU_ASSERT_STRING_EQUAL(matriz[1], "A#Cd");
    CU_ASSERT_STRING_EQUAL(matriz[2], "aBcd");
    CU_ASSERT_STRING_EQUAL(matriz[3], "abcd");

    liberarHistorico(hp);
    free(matriz[0]); free(matriz[1]); free(matriz[2]); free(matriz[3]); free(matriz);
}

void test_executarComandoa_caso2() {
    int L = 3, C = 3;
    char **matriz = malloc(L * sizeof(char *));
    matriz[0] = strdup("a#a");
    matriz[1] = strdup("b#b");
    matriz[2] = strdup("c#c");
    
    // Inicializar o histórico para o teste
    HistoricoPilha *hp = inicializarHistorico();

    executarComandoa(hp, L, C, matriz);

    // Verificar o estado final esperado
    CU_ASSERT_STRING_EQUAL(matriz[0], "A#A");
    CU_ASSERT_STRING_EQUAL(matriz[1], "B#B");
    CU_ASSERT_STRING_EQUAL(matriz[2], "C#C");

    liberarHistorico(hp);
    free(matriz[0]); free(matriz[1]); free(matriz[2]); free(matriz);
}

void test_executarComandov() {
    int L = 3, C = 3;
    char **matriz = malloc(L * sizeof(char *));
    matriz[0] = strdup("ADC");
    matriz[1] = strdup("B#A");
    matriz[2] = strdup("CBD");

    // Executar o comando de verificação
    CU_ASSERT_TRUE(executarComandov(L, C, matriz));

    free(matriz[0]); free(matriz[1]); free(matriz[2]);
    
    matriz = malloc(L * sizeof(char *));
    matriz[0] = strdup("ccA");
    matriz[1] = strdup("B#A");
    matriz[2] = strdup("CBD");

    CU_ASSERT_FALSE(executarComandov(L, C, matriz));

    free(matriz[0]); free(matriz[1]); free(matriz[2]);
    
    matriz = malloc(L * sizeof(char *));
    matriz[0] = strdup("CCA");
    matriz[1] = strdup("B#A");
    matriz[2] = strdup("CBD");

    CU_ASSERT_FALSE(executarComandov(L, C, matriz));

    free(matriz[0]); free(matriz[1]); free(matriz[2]); free(matriz);
}

void test_executarComandoa_caso3() {
    int L = 3, C = 3;
    char **matriz = malloc(L * sizeof(char *));
    matriz[0] = strdup("abc");
    matriz[1] = strdup("def");
    matriz[2] = strdup("ghi");
    
    // Inicializar o histórico para o teste
    HistoricoPilha *hp = inicializarHistorico();

    executarComandoa(hp, L, C, matriz);

    // Verificar o estado final esperado (nenhuma mudança, pois não há letras riscadas)
    CU_ASSERT_STRING_EQUAL(matriz[0], "abc");
    CU_ASSERT_STRING_EQUAL(matriz[1], "def");
    CU_ASSERT_STRING_EQUAL(matriz[2], "ghi");

    liberarHistorico(hp);
    free(matriz[0]); free(matriz[1]); free(matriz[2]); free(matriz);
}

void test_isoladoOrtogonais() {
    int L = 3, C = 3;
    char **matriz = malloc(L * sizeof(char *));
    matriz[0] = strdup("##C");
    matriz[1] = strdup("B#A");
    matriz[2] = strdup("#BA");

    // Verificar se uma célula está isolada
    CU_ASSERT_TRUE(isoladoOrtogonais(0, 0, L, C, matriz)); // A está isolado
    CU_ASSERT_FALSE(isoladoOrtogonais(1, 1, L, C, matriz)); //  não está isolado

    free(matriz[0]); free(matriz[1]); free(matriz[2]);
    
    matriz = malloc(L * sizeof(char *));
    matriz[0] = strdup("#DC");
    matriz[1] = strdup("B#A");
    matriz[2] = strdup("#BA");

    CU_ASSERT_TRUE(isoladoOrtogonais(1, 0, L, C, matriz)); // B está isolado

    free(matriz[0]); free(matriz[1]); free(matriz[2]); free(matriz);
}

void test_vizinhaFicaIsolada() {
    int L = 3, C = 3;
    char **matriz = malloc(L * sizeof(char *));
    matriz[0] = strdup("A#C");
    matriz[1] = strdup("BDA");
    matriz[2] = strdup("CBA");

    // Verificar se uma célula vizinha ficaria isolada
    CU_ASSERT_TRUE(vizinhaFicaIsolada(1, 0, L, C, matriz)); // A ficaria isolado
    CU_ASSERT_FALSE(vizinhaFicaIsolada(1, 1, L, C, matriz)); // D não causa isolamento

    free(matriz[0]); free(matriz[1]); free(matriz[2]);
    
    matriz = malloc(L * sizeof(char *));
    matriz[0] = strdup("#DC");
    matriz[1] = strdup("BCA");
    matriz[2] = strdup("#BA");

    CU_ASSERT_TRUE(vizinhaFicaIsolada(1, 1, L, C, matriz)); // B ficaria isolado
    CU_ASSERT_FALSE(vizinhaFicaIsolada(1, 2, L, C, matriz)); // A nao causa isolamento

    free(matriz[0]); free(matriz[1]); free(matriz[2]); free(matriz);
}

void test_comandoDesfazer() {
    int L = 3, C = 3;
    char **matriz = malloc(L * sizeof(char *));
    matriz[0] = strdup("abc");
    matriz[1] = strdup("def");
    matriz[2] = strdup("ghi");
    
    // Inicializar o histórico para o teste
    HistoricoPilha *hp = inicializarHistorico();

    // Empilhar o estado inicial
    empilharComando(hp, 'r', L, C, matriz);

    // Modificar a matriz
    matriz[0][0] = '#';
    matriz[1][1] = 'E';

    // Desfazer o comando
    desfazerComando(hp, &matriz, &L, &C);

    // Verificar se o estado foi restaurado
    CU_ASSERT_STRING_EQUAL(matriz[0], "abc");
    CU_ASSERT_STRING_EQUAL(matriz[1], "def");
    CU_ASSERT_STRING_EQUAL(matriz[2], "ghi");

    // Testar desfazer sem comandos na pilha
    desfazerComando(hp, &matriz, &L, &C); // Não deve causar erro

    liberarHistorico(hp);
    free(matriz[0]); free(matriz[1]); free(matriz[2]); free(matriz);
}

void test_executarComandoa() {
    int L = 3, C = 3;
    char **matriz = malloc(L * sizeof(char *));
    matriz[0] = strdup("acb");
    matriz[1] = strdup("u#t");
    matriz[2] = strdup("abc");

    // Inicializar o histórico para o teste
    HistoricoPilha *hp = inicializarHistorico();

    // Executar o comando 'a'
    executarComandoa(hp, L, C, matriz);

    // Verificar o estado final esperado
    CU_ASSERT_STRING_EQUAL(matriz[0], "aCb");
    CU_ASSERT_STRING_EQUAL(matriz[1], "U#T");
    CU_ASSERT_STRING_EQUAL(matriz[2], "aBc");

    liberarHistorico(hp);
    free(matriz[0]); free(matriz[1]); free(matriz[2]); free(matriz);
}


int main() {
    CU_initialize_registry();

    CU_pSuite suite = CU_add_suite("Suite Jogo", NULL, NULL);

    CU_add_test(suite, "Test determinarCoordenadas", test_determinarCoordenadas);
    CU_add_test(suite, "Test salvar e carregar jogo", test_salvar_e_carregar_jogo);
    CU_add_test(suite, "Test executarComandoR e B", test_executarComandoR_B);
    CU_add_test(suite, "Test printMatriz", test_printMatriz);
    CU_add_test(suite, "Test empilhar e desfazer comando", test_empilhar_e_desfazer_comando);
    CU_add_test(suite, "Test resolverJogo", test_resolverJogo);
    CU_add_test(suite, "Test executarComandoa caso 1", test_executarComandoa_caso1);
    CU_add_test(suite, "Test executarComandoa caso 2", test_executarComandoa_caso2);
    CU_add_test(suite, "Test executarComandoa caso 3", test_executarComandoa_caso3);
    CU_add_test(suite, "Test executarComandov", test_executarComandov);
    CU_add_test(suite, "Test isoladoOrtogonais", test_isoladoOrtogonais);
    CU_add_test(suite, "Test vizinhaFicaIsolada", test_vizinhaFicaIsolada);
    CU_add_test(suite, "Test comandoDesfazer", test_comandoDesfazer);
    CU_add_test(suite, "Test executarComandoa", test_executarComandoa);

    CU_basic_set_mode(CU_BRM_VERBOSE);
    CU_basic_run_tests();
    CU_cleanup_registry();

    return 0;
}