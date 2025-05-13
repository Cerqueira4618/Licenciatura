#include <stdio.h>
#include <ctype.h> // Para usar a função toupper
#include <string.h> // Para usar a função strcmp
#include <stdlib.h>

// Estrutura para armazenar o histórico de comandos
typedef struct {
    char tipoComando;  // Tipo do comando ('r', 'b', 'g', 'l', etc.)
    char **estadoMatriz; // Estado completo da matriz antes do comando
    int L, C;          // Dimensões da matriz no momento do comando
} Historico;

// Estrutura para encapsular a pilha de histórico
typedef struct {
    Historico *pilha;
    int topo;
} HistoricoPilha;

void liberarMatriz(int L, char **matriz) {
    if (matriz == NULL) return;
    
    for (int i = 0; i < L; i++) {
        if (matriz[i] != NULL) {
            free(matriz[i]);
        }
    }
    free(matriz);
}


// Inicializar a pilha do histórico
HistoricoPilha* inicializarHistorico() {
    HistoricoPilha *hp = malloc(sizeof(HistoricoPilha));
    if (hp == NULL) {
        printf("Erro ao alocar memória para o histórico!\n");
        exit(1);
    }
    hp->pilha = NULL;
    hp->topo = -1;
    return hp;
}

// Liberar a pilha do histórico
void liberarHistorico(HistoricoPilha *hp) {
    if (hp == NULL) return;
    
    // Liberar todos os estados salvos
    for (int i = 0; i <= hp->topo; i++) {
        liberarMatriz(hp->pilha[i].L, hp->pilha[i].estadoMatriz);
    }
    
    free(hp->pilha);
    free(hp);
}

// Funcao para salvar o estado atual da matriz na pilha
char **copiarMatriz(int L, int C, char **carac) {
    if (carac == NULL) return NULL;
    
    char **copia = malloc(L * sizeof(char *));
    if (copia == NULL) return NULL;
    
    for (int i = 0; i < L; i++) {
        copia[i] = malloc((C + 1) * sizeof(char));
        if (copia[i] == NULL) {
            // Liberar memória já alocada em caso de falha
            for (int j = 0; j < i; j++) {
                free(copia[j]);
            }
            free(copia);
            return NULL;
        }
        strcpy(copia[i], carac[i]);
    }
    return copia;
}


void empilharComando(HistoricoPilha *hp, char tipoComando, int L, int C, char **carac) {
    hp->topo++;
    hp->pilha = realloc(hp->pilha, (hp->topo + 1) * sizeof(Historico));
    if (hp->pilha == NULL) {
        printf("Erro ao alocar memória para o histórico de comandos!\n");
        exit(1);
    }
    hp->pilha[hp->topo].tipoComando = tipoComando;
    hp->pilha[hp->topo].L = L;
    hp->pilha[hp->topo].C = C;
    hp->pilha[hp->topo].estadoMatriz = copiarMatriz(L, C, carac);
}

void printMatriz(int L, int C, char **carac) {
    for (int i = 0; i < L; i++) {
        for (int j = 0; j < C && carac[i][j] != '\0'; j++) {
            printf("%c ", carac[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

void salvarJogo(HistoricoPilha *hp, char *nomeArquivo, int L, int C, char **carac) {
    FILE *arquivo = fopen(nomeArquivo, "w");
    if (arquivo == NULL) {
        printf("Erro ao abrir o arquivo para salvar o jogo!\n");
        return;
    }

    // Salvar as dimensões da matriz
    fprintf(arquivo, "%d %d\n", L, C);

    // Salvar o conteúdo da matriz
    for (int i = 0; i < L; i++) {
        fprintf(arquivo, "%s\n", carac[i]);
    }
    
    // Salvar o estado atual no histórico
    empilharComando(hp, 'g', L, C, carac);

    fclose(arquivo);
    printf("\nJogo salvo com sucesso no arquivo '%s'.\n\n", nomeArquivo);
}

void salvarJogoSem(char *nomeArquivo, int L, int C, char **carac) {
    FILE *arquivo = fopen(nomeArquivo, "w");
    if (arquivo == NULL) {
        printf("Erro ao abrir o arquivo para salvar o jogo!\n");
        return;
    }

    // Salvar as dimensões da matriz
    fprintf(arquivo, "%d %d\n", L, C);

    // Salvar o conteúdo da matriz
    for (int i = 0; i < L; i++) {
        fprintf(arquivo, "%s\n", carac[i]);
    }
    
    fclose(arquivo);
    printf("\nJogo salvo com sucesso no arquivo '%s'.\n\n", nomeArquivo);
}

int carregarJogo(char *nomeArquivo, int *L, int *C, char ***carac) {
    FILE *arquivo = fopen(nomeArquivo, "r");
    if (arquivo == NULL) {
        printf("Erro ao abrir o arquivo para carregar o jogo!\n");
        return 0; // Retorna 0 para indicar falha
    }

    // Ler as dimensões da matriz
    if (fscanf(arquivo, "%d %d\n", L, C) != 2) {
        printf("Erro ao ler as dimensões do jogo no arquivo!\n");
        fclose(arquivo);
        return 0;
    }
    printf("Dimensões lidas: L = %d, C = %d\n", *L, *C);

    // Liberar a matriz atual, se já estiver alocada
    if (*carac != NULL) {
        liberarMatriz(*L, *carac);
        *carac = NULL; // Atualizar o ponteiro para evitar uso após liberação
    }

    // Alocar memória para a nova matriz
    *carac = malloc(*L * sizeof(char *));
    if (*carac == NULL) {
        printf("Erro ao alocar memória para a matriz!\n");
        fclose(arquivo);
        return 0;
    }

    for (int i = 0; i < *L; i++) {
        (*carac)[i] = malloc((*C + 1) * sizeof(char));
        if ((*carac)[i] == NULL) {
            printf("Erro ao alocar memória para a linha %d da matriz!\n", i);
            liberarMatriz(i, *carac);
            fclose(arquivo);
            return 0;
        }
    }

    // Ler o conteúdo da matriz
    for (int i = 0; i < *L; i++) {
        if (fscanf(arquivo, "%s", (*carac)[i]) != 1) {
            printf("Erro ao ler a linha %d do jogo no arquivo!\n", i);
            liberarMatriz(*L, *carac);
            fclose(arquivo);
            return 0;
        }
        printf("Linha %d lida: %s\n", i, (*carac)[i]); // Log de depuração
    }

    fclose(arquivo);
    printf("\nJogo carregado com sucesso do arquivo '%s'.\n\n", nomeArquivo);
    return 1; // Retorna 1 para indicar sucesso
}

// Função para determinar as coordenadas da matriz a partir de uma entrada no formato "<letra><número>"
int determinarCoordenadas(char *coordenada, int *x, int *y) {
    if (strlen(coordenada) < 2) {
        printf("Coordenada inválida!\n");
        return 0; // Retorna 0 para indicar falha
    }

    // A primeira parte da coordenada é a letra (linha)
    char linha = coordenada[0];
    if (!islower(linha)) { // Verifica se a letra não é minúscula
        printf("Coordenada inválida!\n");
        return 0;
    }

    // A segunda parte da coordenada é o número (coluna)
    int coluna = atoi(&coordenada[1]); // Converte a parte numérica para inteiro
    if (coluna <= 0) {
        printf("Coordenada inválida!\n");
        return 0;
    }

    // Converte a letra para o índice da linha (0-indexado)
    *x = linha - 'a';

    // Converte o número para o índice da coluna (0-indexado)
    *y = coluna - 1;

    return 1; // Retorna 1 para indicar sucesso
}

void executarComandoR(HistoricoPilha *hp, int L, int C, char **carac, char *comando) {
    char coordenada[10];
    if (sscanf(comando, "r %s", coordenada) != 1) {
        printf("Comando inválido!\n");
        return;
    }

    int x, y;
    if (!determinarCoordenadas(coordenada, &x, &y)) {
        printf("Erro ao determinar as coordenadas!\n");
        return;
    }

    if (x < 0 || x >= L || y < 0 || y >= C) {
        printf("Coordenadas fora dos limites da matriz!\n");
        return;
    }

    // Salvar o estado atual no histórico
    empilharComando(hp, 'r', L, C, carac);

    // Modificar a matriz
    carac[x][y] = '#';
    printf("Posição (%c, %d) alterada para '#'.\n", 'a' + x, y + 1);
}

void executarComandoB(HistoricoPilha *hp, int L, int C, char **carac, char *comando) {
    char coordenada[10];
    if (sscanf(comando, "b %s", coordenada) != 1) {
        printf("Comando inválido!\n");
        return;
    }

    int x, y;
    if (!determinarCoordenadas(coordenada, &x, &y)) {
        printf("Erro ao determinar as coordenadas!\n");
        return;
    }

    if (x < 0 || x >= L || y < 0 || y >= C) {
        printf("Coordenadas fora dos limites da matriz!\n");
        return;
    }

    // Salvar o estado atual no histórico
    empilharComando(hp, 'b', L, C, carac);

    // Modificar a matriz
    carac[x][y] = toupper(carac[x][y]);
    printf("Posição (%c, %d) alterada para '%c'.\n", 'a' + x, y + 1, carac[x][y]);
}

int temLetrasOrtogonais(int i, int j, char **carac, int L, int C) {
    int resultado = 0;
    // Verificar se a posição atual está dentro dos limites e não é riscada
    if (i < 0 || i >= L || j < 0 || j >= C || carac[i][j] == '#') return 0;

    // Verificar as posições ortogonais (cima, baixo, esquerda, direita)
    if (i > 0 && carac[i - 1][j] != '#' && isalpha(carac[i - 1][j])) resultado = 1; // Cima
    if (i < L - 1 && carac[i + 1][j] != '#' && isalpha(carac[i + 1][j])) resultado = 1; // Baixo
    if (j > 0 && carac[i][j - 1] != '#' && isalpha(carac[i][j - 1])) resultado = 1; // Esquerda
    if (j < C - 1 && carac[i][j + 1] != '#' && isalpha(carac[i][j + 1])) resultado = 1; // Direita

    return resultado; // Não há letras nas posições ortogonais
}

int isoladoOrtogonais(int i, int j, int L, int C, char **carac) {
    // Verificar se a posição está isolada (nenhuma direção tem letras válidas)
    if (!temLetrasOrtogonais(i - 1, j, carac, L, C) && // Cima
        !temLetrasOrtogonais(i + 1, j, carac, L, C) && // Baixo
        !temLetrasOrtogonais(i, j - 1, carac, L, C) && // Esquerda
        !temLetrasOrtogonais(i, j + 1, carac, L, C)) { // Direita
        return 1; // Está isolado
    }
    return 0; // Não está isolado
}

int vizinhaFicaIsolada(int i, int j, int L, int C, char **carac) {
    // CIMA
    if (i > 0 && isalpha(carac[i - 1][j]) && carac[i - 1][j] != '#') {
        char original = carac[i][j];
        carac[i][j] = '#';
        int isolada = isoladoOrtogonais(i - 1, j, L, C, carac);
        carac[i][j] = original;
        if (isolada) return 1;
    }

    // BAIXO
    if (i < L - 1 && isalpha(carac[i + 1][j]) && carac[i + 1][j] != '#') {
        char original = carac[i][j];
        carac[i][j] = '#';
        int isolada = isoladoOrtogonais(i + 1, j, L, C, carac);
        carac[i][j] = original;
        if (isolada) return 1;
    }

    // ESQUERDA
    if (j > 0 && isalpha(carac[i][j - 1]) && carac[i][j - 1] != '#') {
        char original = carac[i][j];
        carac[i][j] = '#';
        int isolada = isoladoOrtogonais(i, j - 1, L, C, carac);
        carac[i][j] = original;
        if (isolada) return 1;
    }
    
    // DIREITA
    if (j < C - 1 && isalpha(carac[i][j + 1]) && carac[i][j + 1] != '#') {
        char original = carac[i][j];
        carac[i][j] = '#';
        int isolada = isoladoOrtogonais(i, j + 1, L, C, carac);
        carac[i][j] = original;
        if (isolada) return 1;
    }

    return 0; // Nenhuma vizinha ficaria isolada
}

void executarComandoasem(int L, int C, char **carac) {
    int k, i, j;

    // 1. Riscar todas as letras iguais a uma letra branca na mesma linha e/ou coluna
    for (i = 0; i < L; i++) {
        for (j = 0; j < C; j++) {
            if (isupper(carac[i][j])) {
                char z = tolower(carac[i][j]);
                for (k = 0; k < L; k++) {
                    if (carac[k][j] == z) {
                        carac[k][j] = '#';
                    }
                }
                for (k = 0; k < C; k++) {
                    if (carac[i][k] == z) {
                        carac[i][k] = '#';
                    }
                }
            }
        }
    }

    // 2. Pintar de branco todas as casas vizinhas de uma casa riscada
    for (i = 0; i < L; i++) {
        for (j = 0; j < C; j++) {
            if (carac[i][j] == '#') {
                if (i + 1 < L && islower(carac[i + 1][j])) carac[i + 1][j] = toupper(carac[i + 1][j]);
                if (i - 1 >= 0 && islower(carac[i - 1][j])) carac[i - 1][j] = toupper(carac[i - 1][j]);
                if (j + 1 < C && islower(carac[i][j + 1])) carac[i][j + 1] = toupper(carac[i][j + 1]);
                if (j - 1 >= 0 && islower(carac[i][j - 1])) carac[i][j - 1] = toupper(carac[i][j - 1]);
            }
        }
    }

    for (i = 0; i < L; i++) {
        for (j = 0; j < C; j++) {
            if (carac[i][j] != '#' && islower(carac[i][j])) {
                if (vizinhaFicaIsolada(i, j, L, C, carac)) {
                    carac[i][j] = toupper(carac[i][j]);
                }
            }
        }
    }
}

void executarComandoa(HistoricoPilha *hp, int L, int C, char **carac) {
    // Salvar o estado atual no histórico
    empilharComando(hp, 'a', L, C, carac);

    // Executar o comando sem salvar no histórico
    executarComandoasem(L, C, carac);
}

void executarComandoAsem(int L, int C, char **carac) {
    int mudou = 1; // Inicialmente, assumimos que haverá mudanças

    // Enquanto houver mudanças no tabuleiro
    while (mudou) {
        mudou = 0; // Resetar a variável de controle

        // Copiar o estado atual da matriz para comparação
        char **estadoAnterior = copiarMatriz(L, C, carac);
        if (estadoAnterior == NULL) {
            printf("Erro ao alocar memória para a cópia da matriz!\n");
            return;
        }

        // Executar o comando `a`
        executarComandoasem(L, C, carac);

        // Verificar se houve mudanças no tabuleiro
        for (int i = 0; i < L; i++) {
            if (strcmp(estadoAnterior[i], carac[i]) != 0) {
                mudou = 1; // Detectamos mudanças
                break;
            }
        }

        // Liberar a memória da cópia do estado anterior
        liberarMatriz(L, estadoAnterior);
    }
}

void executarComandoA(HistoricoPilha *hp, int L, int C, char **carac) {
    // Salvar o estado inicial no histórico
    empilharComando(hp, 'A', L, C, carac);

    // Executar o comando sem salvar no histórico
    executarComandoAsem(L, C, carac);

    printf("Comando 'A' executado: o tabuleiro foi atualizado até estabilizar.\n");
}

int executarComandov(int L, int C, char **carac) {
    int i, j;
    int tudocerto = 1;
    int iguais = 0;

    for (i = 0; i < L; i++) {
        for (j = 0; j < C; j++) {
            if (carac[i][j] == '#') {
                // Verificar vizinho acima
                if (i > 0 && carac[i - 1][j] == '#') {
                    tudocerto = 0;
                    printf("Restrição violada: '#' na linha %d e coluna %d está junto com '#' acima.\n", i + 1, j + 1);
                }
                // Verificar vizinho abaixo
                if (i < L - 1 && carac[i + 1][j] == '#') {
                    tudocerto = 0;
                    printf("Restrição violada: '#' na linha %d e coluna %d está junto com '#' abaixo.\n", i + 1, j + 1);
                }
                // Verificar vizinho à esquerda
                if (j > 0 && carac[i][j - 1] == '#') {
                    tudocerto = 0;
                    printf("Restrição violada: '#' na linha %d e coluna %d está junto com '#' à esquerda.\n", i + 1, j + 1);
                }
                // Verificar vizinho à direita
                if (j < C - 1 && carac[i][j + 1] == '#') {
                    tudocerto = 0;
                    printf("Restrição violada: '#' na linha %d e coluna %d está junto com '#' à direita.\n", i + 1, j + 1);
                }
                // Verifica se as letras ao lado de '#' são maiúsculas
                if (i + 1 < L && isalpha(carac[i + 1][j]) && !isupper(carac[i + 1][j])) {
                    tudocerto = 0;
                    printf("Restrição violada na linha %d e coluna %d (abaixo).\n", i + 2, j + 1);
                }
                if (i - 1 >= 0 && isalpha(carac[i - 1][j]) && !isupper(carac[i - 1][j])) {
                    tudocerto = 0;
                    printf("Restrição violada na linha %d e coluna %d (acima).\n", i, j + 1);
                }
                if (j + 1 < C && isalpha(carac[i][j + 1]) && !isupper(carac[i][j + 1])) {
                    tudocerto = 0;
                    printf("Restrição violada na linha %d e coluna %d (à direita).\n", i + 1, j + 2);
                }
                if (j - 1 >= 0 && isalpha(carac[i][j - 1]) && !isupper(carac[i][j - 1])) {
                    tudocerto = 0;
                    printf("Restrição violada na linha %d e coluna %d (à esquerda).\n", i + 1, j);
                }
            }

            // Verifica se existem letras iguais na mesma linha ou coluna
            if (isupper(carac[i][j])) {
                char letra = carac[i][j];

                // Verifica na mesma linha
                for (int k = 0; k < C; k++) {
                    if (k != j && carac[i][k] == letra) {
                        tudocerto = 0;
                        iguais = 1;
                    }
                }

                // Verifica na mesma coluna
                for (int k = 0; k < L; k++) {
                    if (k != i && carac[k][j] == letra) {
                        tudocerto = 0;
                        iguais = 1;
                    }
                }
            }

            // Verifica se ao lado de uma letra maiúscula existe pelo menos uma outra maiúscula
            if (isupper(carac[i][j])) {
                int temMaiuscula = 0;

                if (i + 1 < L && isalpha(carac[i + 1][j])) temMaiuscula = 1; // Abaixo
                if (i - 1 >= 0 && isalpha(carac[i - 1][j])) temMaiuscula = 1; // Acima
                if (j + 1 < C && isalpha(carac[i][j + 1])) temMaiuscula = 1; // Direita
                if (j - 1 >= 0 && isalpha(carac[i][j - 1])) temMaiuscula = 1; // Esquerda

                if (!temMaiuscula) {
                    tudocerto = 0;
                    printf("Restrição violada: letra maiúscula na linha %d e coluna %d não tem nenhum caminho ortogonal.\n", i + 1, j + 1);
                }
            }
        }
    }
    
    if (iguais) printf("Existem letras repetidas na mesma linha ou coluna.\n");
    if (tudocerto) printf("Não foi violada nenhuma das restrições.\n");
    return tudocerto;
}

// Função para desfazer o último comando
void desfazerComando(HistoricoPilha *hp, char ***carac, int *L, int *C) {
    if (hp->topo == -1) {
        printf("Nenhum comando para desfazer!\n");
        return;
    }

    // Recuperar o último comando
    Historico ultimo = hp->pilha[hp->topo];

    // Verifica se o comando é 'v'
    if (ultimo.tipoComando == 'v') {
        printf("O comando 'v' não pode ser desfeito!\n");
        return;
    }

    hp->topo--;

    // Liberar a matriz atual
    liberarMatriz(*L, *carac);

    // Restaurar o estado anterior da matriz
    *carac = ultimo.estadoMatriz;
    *L = ultimo.L;
    *C = ultimo.C;

    printf("Comando '%c' desfeito. Estado anterior restaurado.\n", ultimo.tipoComando);
}

void procurarPadrãoABA(int L, int C, char **carac) {
    for (int i = 0; i < L; i++) {
        for (int j = 1; j < C - 1; j++) {
            // Verificar padrão ABA na linha
            if (carac[i][j - 1] == carac[i][j + 1] && islower(carac[i][j - 1]) && carac[i][j] == '.') {
                carac[i][j] = toupper(carac[i][j - 1]);
            }
        }
    }

    for (int j = 0; j < C; j++) {
        for (int i = 1; i < L - 1; i++) {
            // Verificar padrão ABA na coluna
            if (carac[i - 1][j] == carac[i + 1][j] && islower(carac[i - 1][j]) && carac[i][j] == '.') {
                carac[i][j] = toupper(carac[i - 1][j]);
            }
        }
    }
}

void procurarPadrãoAAA(int L, int C, char **carac) {
    for (int i = 0; i < L; i++) {
        for (int j = 2; j < C; j++) {
            // Verificar padrão AA.A na linha
            if (carac[i][j - 2] == carac[i][j - 1] && islower(carac[i][j - 2]) && carac[i][j] == '.') {
                carac[i][j] = '#';
            }
        }
    }

    for (int j = 0; j < C; j++) {
        for (int i = 2; i < L; i++) {
            // Verificar padrão AA.A na coluna
            if (carac[i - 2][j] == carac[i - 1][j] && islower(carac[i - 2][j]) && carac[i][j] == '.') {
                carac[i][j] = '#';            
            }
        }
    }
}

void resolverJogo(HistoricoPilha *hp, int L, int C, char **carac) {
    // Guardar uma cópia da matriz original antes de resolver
    char **original = copiarMatriz(L, C, carac);
    if (original == NULL) {
        printf("Erro ao alocar memória para a cópia da matriz.\n");
        return;
    }
    
    // Salvar o estado inicial no histórico
    empilharComando(hp, 'R', L, C, carac);

    carregarJogo("res.txt", &L, &C, &original);
    printf("Jogo carregado do ficheiro 'res.txt'. Iniciando resolução...\n");

    // Procurar padrões e aplicar regras para resolver o jogo
    int mudou = 1;
    while (mudou) {
        mudou = 0;
        
        // 1. Procurar padrões ABA e AA.A
        procurarPadrãoABA(L, C, original);
        procurarPadrãoAAA(L, C, original);

        // 2. Usar o comando A para estabilizar o tabuleiro
        executarComandoAsem(L, C, original);

        // 3. Procurar letras repetidas em linhas ou colunas
        for (int i = 0; i < L; i++) {
            for (int j = 0; j < C; j++) {
                if (islower(original[i][j])) {
                    char simbolo = original[i][j];
                    int unico = 1;

                    // Verificar se o símbolo é único na linha
                    for (int k = 0; k < C; k++) {
                        if (k != j && original[i][k] == simbolo) {
                            unico = 0;
                            break;
                        }
                    }

                    // Verificar se o símbolo é único na coluna
                    for (int k = 0; k < L; k++) {
                        if (k != i && original[k][j] == simbolo) {
                            unico = 0;
                            break;
                        }
                    }

                    // Se for único, pintar de branco
                    if (unico) {
                        original[i][j] = toupper(original[i][j]);
                        mudou = 1;

                        // Usar o comando A após pintar
                        executarComandoAsem(L, C, original);

                        // Verificar se há contradições
                        if (!executarComandov(L, C, original)) {
                            printf("Contradição encontrada ao pintar. Voltando atrás...\n");
                            liberarMatriz(L, original);
                            desfazerComando(hp, &carac, &L, &C);
                            return;
                        }
                    } else {
                        // Se não for único, tentar riscar
                        original[i][j] = '#';
                        mudou = 1;

                        // Usar o comando A após riscar
                        executarComandoAsem(L, C, original);

                        // Verificar se há contradições
                        if (!executarComandov(L, C, original)) {
                            printf("Contradição encontrada ao riscar. Voltando atrás...\n");
                            liberarMatriz(L, original);
                            desfazerComando(hp, &original, &L, &C);
                            return;
                        }
                    }
                }
            }
        }

        // Verificar se ainda há mudanças possíveis
        for (int i = 0; i < L; i++) {
            for (int j = 0; j < C; j++) {
                if (original[i][j] == '.') {
                    mudou = 1;
                    break;
                }
            }
            if (mudou) break;
        }
    }

    printf("Jogo resolvido automaticamente.\n");

    // Salvar o estado resolvido no ficheiro res.txt
    salvarJogoSem("res.txt", L, C, original);
    
    // Liberar a memória da cópia original antes de sair
    liberarMatriz(L, original);
}