# Documentação Técnica COBOL

## 📋 Identificação do Programa

| Atributo | Valor |
|----------|-------|
| **Nome do Programa** | CBACT01C |
| **Tipo** | BATCH |
| **Aplicação** | CardDemo |
| **Autor** | AWS |
| **Versão** | v1.0 (Implícito) |
| **Data da Versão** | N/A (Não especificado, extraído da estrutura de cabeçalho) |
| **Linguagem** | COBOL Enterprise |

**Descrição Resumida:**  
Programa de processamento em lote (BATCH) da aplicação CardDemo. Seu principal objetivo é ler sequencialmente o arquivo mestre de contas (ACCTFILE) e simplesmente reescrever cada registro lido no arquivo de saída principal (OUTFILE), servindo como uma rotina de extração ou cópia.

**Função Principal:**  
Ler cada registro do arquivo de contas (ACCTFILE) e gravá-lo no arquivo de saída (OUTFILE).

## 📊 ARQUITETURA DO SISTEMA

### Diagrama de Fluxo de Dados

```
┌─────────────────┐
│  ARQUIVO DE     │
│    ENTRADA      │
│    (ACCTFILE)   │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│   PROGRAMA      │
│   CBACT01C      │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  ARQUIVOS DE    │
│     SAÍDA       │
│    (OUTFILE)    │
└─────────────────┘
```

### Resumo de I/O

| Tipo | Qtd | Nomes |
|------|-----|-------|
| Entrada | 1 | ACCTFILE |
| Saída | 1 | OUTFILE |
| Entrada/Saída | 0 | Nenhum |

## 📁 CATÁLOGO DE ARQUIVOS

### 🔵 ARQUIVOS DE ENTRADA

#### [1] ACCTFILE - Arquivo de Contas Mestre

| Propriedade | Especificação |
|-------------|---------------|
| DDName | ACCTFILE |
| Organização | SEQUENTIAL |
| Tipo VSAM | N/A |
| Modo de Acesso | SEQUENTIAL |
| Record Key | N/A (Não sequencial por chave) |
| File Status | ACCTFILE-STATUS |
| Tamanho do Registro | 240 bytes (Implícito pelo layout do WS-ACCT-FILE-REC) |

**Layout do Registro:**

O layout completo é carregado através do copybook ACREC01C. Os campos principais são:

| Pos | Campo | Tipo | Tamanho | Formato | Descrição |
|-----|-------|------|---------|---------|-----------|
| 001-016 | ACREC-ACCT-ID | 9(16) | 16 | DISPLAY | ID da conta (Número do cartão) |
| 017-020 | ACREC-ACCT-STATUS | 9(04) | 04 | DISPLAY | Status da conta |
| 021-026 | ACREC-ISSUE-DT | X(06) | 06 | DISPLAY | Data de emissão (AAAAMMDD) |
| 027-040 | ACREC-CURR-BAL | S9(11)V99 | 14 | COMP-3 | Saldo atual (Packed Decimal) |
| 041-240 | FILLER | X(200) | 200 | DISPLAY | Preenchimento do registro |

**Propósito:**
Contém todos os dados mestres das contas do CardDemo. O programa lê este arquivo para extrair e retransmitir esses dados para o arquivo de saída.

### 🟢 ARQUIVOS DE SAÍDA

#### [1] OUTFILE - Arquivo de Saída Principal

| Propriedade | Especificação |
|-------------|---------------|
| DDName | OUTFILE |
| Organização | SEQUENTIAL |
| Modo de Acesso | SEQUENTIAL |
| File Status | OUTFILE-STATUS |
| Tipo de Registro | FIXED |
| Tamanho do Registro | 240 bytes |
| Recording Mode | F (Fixed) |

**Layout do Registro:**

O registro de saída é uma cópia exata do registro de entrada (ACCTFILE), pois os dados são movidos diretamente do WS-ACCT-FILE-REC para o OUTFILE-REC.

| Pos | Campo | Tipo | Tamanho | Formato | Descrição |
|-----|-------|------|---------|---------|-----------|
| 001-240 | OUTFILE-REC | X(240) | 240 | DISPLAY | Registro de saída, espelha o registro de entrada (ACREC01C). |

**Propósito:**
Receber o fluxo de registros processados do arquivo de contas. Atua como o arquivo de saída primário do processamento em lote.

**Observações Especiais:**

O programa utiliza `WRITE OUTFILE-REC` (Linha 80200). O conteúdo do registro é preparado por um `MOVE WS-ACCT-FILE-REC TO OUTFILE-REC` (Linha 70300).

## 📚 COPYBOOKS E INCLUDES

### ACREC01C

| Propriedade | Valor |
|-------------|-------|
| Nome | ACREC01C |
| Localização no Código | Linha 40100 |
| Tipo | Estrutura de Dados (Layout de Registro) |

**Estruturas Definidas:**

01 WS-ACCT-FILE-REC - Buffer de trabalho para o registro lido do ACCTFILE.

**Campos Principais:**

```cobol
       01 WS-ACCT-FILE-REC.
          COPY ACREC01C.
```

**Propósito:**
Define o layout do registro de contas (ACCTFILE). Este copybook é incluído na WORKING-STORAGE SECTION para mapear o registro de entrada e usá-lo como área de trabalho.

### CBEMSG01

| Propriedade | Valor |
|-------------|-------|
| Nome | CBEMSG01 |
| Localização no Código | Linha 50100 |
| Tipo | Variáveis de Status e Códigos de Retorno |

**Estruturas Definidas:**

01 COMM-AREA - Área de comunicação e flags de status.

**Campos Principais:**

```cobol
       01 COMM-AREA.
          COPY CBEMSG01.
```

**Propósito:**
Contém variáveis padrão de controle de status e indicadores (ex: APPL-RESULT, APPL-AOK, APPL-BMSG). É essencial para o controle de fluxo e tratamento de erros do programa.

## 💾 WORKING-STORAGE SECTION

### Variáveis de Controle de Arquivo

| Variável | Tipo | Uso |
|----------|------|-----|
| ACCTFILE-STATUS | PIC XX | Status do arquivo ACCTFILE |
| OUTFILE-STATUS | PIC XX | Status do arquivo OUTFILE |
| IO-STATUS | PIC XX | Status de I/O para exibição de erros |

### Flags e Indicadores

| Variável | Tipo | Valores | Propósito |
|----------|------|---------|-----------|
| END-OF-ACCTFILE | PIC X | 'Y' / 'N' | Flag que indica se o final do arquivo ACCTFILE foi atingido. |

### Códigos de Retorno

| Variável | Valor | Condição | Significado |
|----------|-------|----------|-------------|
| APPL-AOK | 00 | 88-LEVEL | Condição para status de sucesso (geralmente usado após chamadas ou operações de I/O). |
| APPL-ERROR | > 00 | 88-LEVEL | Condição para status de erro. |
| APPL-RESULT | PIC 9(04) | Variável de retorno | Variável principal de código de retorno da aplicação. |

### Áreas de Trabalho

| Área | Estrutura | Propósito |
|------|-----------|-----------|
| WS-COUNTERS | 01 Group | Contadores de registros lidos e gravados. |
| WS-ACCT-FILE-REC | Copy ACREC01C | Buffer de trabalho do registro de entrada. |
| TWO-BYTES-BINARY | PIC 9(04) COMP | Conversão binária para tratamento de erros de VSAM (status '9x'). |

### Constantes e Literais

| Nome | Valor | Uso |
|------|-------|-----|
| COUNT-ACCT-READ | PIC 9(09) | Contador de registros lidos do ACCTFILE. |
| COUNT-ACCT-WRITE | PIC 9(09) | Contador de registros escritos no OUTFILE. |

## 🔄 FLUXO DE EXECUÇÃO

### Visão Geral do Fluxo

```
INÍCIO
│
├─► 0000-PROGRAM-START
│     └─► PERFORM 1000-INITIALIZATION
│           ├─► Abrir arquivos (ACCTFILE e OUTFILE)
│           └─► Inicializar variáveis
│
├─► PERFORM 2000-PROCESS-ACCTFILE
│     ├─► Loop de leitura (PERFORM UNTIL END-OF-ACCTFILE = 'Y')
│     ├─► Leitura do próximo registro (1100-ACCTFILE-READ)
│     └─► Processamento (7000-PROCESS-RECORD) e Escrita (8000-OUTFILE-WRITE)
│
└─► PERFORM 9000-PROGRAM-END
    ├─► Fechar arquivos
    ├─► Exibir estatísticas
    └─► STOP RUN/RETURN
```

### Sequência de Execução

**FASE 1: INICIALIZAÇÃO**
Linha 10000-20000: 1000-INITIALIZATION
│
├─ [Passo 1]: Abrir o arquivo de contas ACCTFILE (Parágrafo 9000-ACCTFILE-OPEN).
├─ [Passo 2]: Abrir o arquivo de saída OUTFILE (Parágrafo 9000-OUTFILE-OPEN).
├─ [Passo 3]: Ler o primeiro registro do ACCTFILE (Parágrafo 1100-ACCTFILE-READ).
└─ [Passo N]: Se a primeira leitura falhar com um erro não-EOF, o programa é abortado.

**FASE 2: PROCESSAMENTO PRINCIPAL**
Linha 20100-20300: 2000-PROCESS-ACCTFILE
│
├─ LOOP PRINCIPAL:
│  ├─ Condição: UNTIL END-OF-ACCTFILE = 'Y' (Linha 20200)
│  ├─ Processamento: PERFORM 7000-PROCESS-RECORD
│  ├─ Leitura: PERFORM 1100-ACCTFILE-READ (dentro de 7000)
│  └─ Loop de Processamento: O 7000-PROCESS-RECORD move o registro lido para a área de saída e escreve (8000-OUTFILE-WRITE).
│
└─ Saída do Loop: END-OF-ACCTFILE é setado para 'Y' após a leitura de EOF (status '10').

**FASE 3: FINALIZAÇÃO**
Linha 30100-30200: 9000-PROGRAM-END
│
├─ [Passo 1]: Fechar o arquivo ACCTFILE (Parágrafo 9000-ACCTFILE-CLOSE).
├─ [Passo 2]: Fechar o arquivo OUTFILE (Parágrafo 9000-OUTFILE-CLOSE).
├─ [Passo 3]: Exibir contadores de leitura e escrita.
└─ [Passo N]: STOP RUN.

## 📝 CATÁLOGO DE PARÁGRAFOS

### 0000-PROGRAM-START

| Propriedade | Detalhe |
|-------------|---------|
| Localização | Linhas 10100-10200 |
| Tipo | Controle Principal |
| Propósito | Ponto de entrada principal do programa. Inicia a sequência de inicialização, processamento e finalização. |

**Lógica Detalhada:**

```cobol
PERFORM 1000-INITIALIZATION.
PERFORM 2000-PROCESS-ACCTFILE.
PERFORM 9000-PROGRAM-END.
STOP RUN.
```

**Parágrafos Invocados:**

- 1000-INITIALIZATION (linha 10100)
- 2000-PROCESS-ACCTFILE (linha 10100)
- 9000-PROGRAM-END (linha 10200)

**Invocado Por:**

N/A (Ponto de entrada do programa)

**Tratamento de Erros:**

Se qualquer parágrafo chamado retornar um erro (via APPL-AOK = FALSO), o programa é abortado implicitamente através do encadeamento de parágrafos de I/O que chamam 9999-ABEND-PROGRAM.

**Variáveis Modificadas:**

Nenhuma modificação direta, apenas controle de fluxo.

### 7000-PROCESS-RECORD

| Propriedade | Detalhe |
|-------------|---------|
| Localização | Linhas 70100-70500 |
| Tipo | Processamento |
| Propósito | Executar a lógica de processamento de um único registro lido. No caso, é apenas a retransmissão do registro. |

**Lógica Detalhada:**

- Move o registro lido (WS-ACCT-FILE-REC) para a área de registro de saída (OUTFILE-REC). (Linha 70300)
- PERFORM 8000-OUTFILE-WRITE para gravar o registro. (Linha 70400)
- PERFORM 1100-ACCTFILE-READ para ler o próximo registro e controlar o loop principal. (Linha 70500)

**Parágrafos Invocados:**

- 8000-OUTFILE-WRITE (linha 70400)
- 1100-ACCTFILE-READ (linha 70500)

**Invocado Por:**

- 2000-PROCESS-ACCTFILE (Linha 20200)

**Observações:**

Este parágrafo implementa a regra de negócio central: Cópia direta dos dados.

### 9999-ABEND-PROGRAM

| Propriedade | Detalhe |
|-------------|---------|
| Localização | Linhas 99990-99993 |
| Tipo | Erro/Finalização |
| Propósito | Terminar o programa de forma anormal (ABEND) com um código específico. |

**Lógica Detalhada:**

- Exibe a mensagem "ABENDING PROGRAM".
- Move 999 para o código de ABEND (ABCODE).
- Chama o serviço CEE3ABD para terminar o programa.

**Parágrafos Invocados:**

N/A (Chama serviço externo CEE3ABD)

**Invocado Por:**

- 9000-ACCTFILE-OPEN (Erro de OPEN)
- 9000-OUTFILE-OPEN (Erro de OPEN)
- 1100-ACCTFILE-READ (Erro de READ não-EOF)
- 8000-OUTFILE-WRITE (Erro de WRITE)
- 9000-ACCTFILE-CLOSE (Erro de CLOSE)
- 9000-OUTFILE-CLOSE (Erro de CLOSE)

## 🔀 OPERAÇÕES DE I/O

### Operações de Leitura

| Arquivo | Parágrafo | Tipo | Tratamento EOF | Status Verificados |
|---------|-----------|------|----------------|-------------------|
| ACCTFILE | 1100-ACCTFILE-READ (L: 11000) | READ | Seta END-OF-ACCTFILE = 'Y' | 00, 10, outros |

**Lógica de Leitura ACCTFILE:**

1. `READ ACCTFILE-FILE INTO WS-ACCT-FILE-REC` (Linha 11000)
2. `IF status = '00' THEN`
   - Adiciona 1 a COUNT-ACCT-READ. (Linha 11300)
3. `ELSE IF status = '10' THEN`
   - `MOVE 'Y' TO END-OF-ACCTFILE`. (Linha 11500)
4. `ELSE`
   - Exibe erro e PERFORM 9999-ABEND-PROGRAM. (Linhas 11600-11900)

### Operações de Escrita

| Arquivo | Parágrafo | Tipo | Validação Prévia | Status Verificados |
|---------|-----------|------|------------------|-------------------|
| OUTFILE | 8000-OUTFILE-WRITE (L: 80100) | WRITE | Sim - Apenas se APPL-AOK | 00, 10, outros |

**Lógica de Escrita OUTFILE:**

1. Adiciona 1 a COUNT-ACCT-WRITE. (Linha 80100)
2. `WRITE OUTFILE-REC` (Linha 80200)
3. `IF status NOT = '00' AND NOT = '10' THEN`
   - Exibe erro e PERFORM 9999-ABEND-PROGRAM. (Linhas 80600-81000)

## 🔧 TRANSFORMAÇÕES E PROCESSAMENTO

### Mapeamento Entrada → Saída

**Registro ACCTFILE (via WS-ACCT-FILE-REC) → Registro OUTFILE (OUTFILE-REC):**

| Campo Origem | Transformação | Campo Destino | Regra |
|--------------|---------------|---------------|-------|
| WS-ACCT-FILE-REC | Direto | OUTFILE-REC | Cópia de 240 bytes. |

### Cálculos e Fórmulas

Nenhum cálculo ou fórmula complexa explícita. O programa realiza apenas cópia de dados.

### Conversões de Formato

| Tipo | Campo | De → Para | Método | Localização |
|------|-------|-----------|--------|-------------|
| Numérico | IO-STATUS | PIC XX → PIC 9(04) (para exibição) | MOVE/COMPUTE | Linha 99100-99400 |

### Valores Padrão (Default Values)

| Campo | Valor Padrão | Condição | Linha |
|-------|--------------|----------|-------|
| END-OF-ACCTFILE | 'N' | Inicialização do programa | Linha 40300 |
| Contadores | 0 | Inicialização do programa | Linha 40500 |

## 📞 CHAMADAS EXTERNAS

O programa utiliza chamadas de sistema para controle de ABEND e I/O estendido:

### CEE3ABD

| Propriedade | Detalhe |
|-------------|---------|
| Nome | CEE3ABD |
| Tipo | Serviço CICS/MVS (ABEND/Stop) |
| Localização | Linha 99993 |
| Invocado em | Parágrafo 9999-ABEND-PROGRAM |

**Parâmetros Passados:**

| Ordem | Nome | Tipo | Direção | Propósito |
|-------|------|------|---------|-----------|
| 1 | ABCODE | PIC 9(03) | INPUT | Código de ABEND (e.g., 999) |
| 2 | TIMING | PIC 9(04) | INPUT | Opção de timing (0 = imediato) |

**Retorno:**

O programa não retorna, pois é terminado.

**Tratamento de Erros:**

Chamado apenas em caso de erro de I/O irrecuperável.

**Propósito da Chamada:**
Garantir uma terminação controlada do programa em caso de falha de I/O.

## ⚠️ TRATAMENTO DE ERROS E EXCEÇÕES

### Matriz de Códigos de Status

| Código | Origem | Significado | Ação Tomada | Parágrafo |
|--------|--------|-------------|-------------|-----------|
| 00 | ACCTFILE/OUTFILE | Sucesso na Operação | Continuar | Diversos |
| 10 | ACCTFILE | End-Of-File (EOF) | Seta END-OF-ACCTFILE para 'Y' | 1100-ACCTFILE-READ |
| 9x | ACCTFILE/OUTFILE | Erro de I/O (VSAM/Geral) | ABEND imediato | Todos os parágrafos de I/O |
| outros | ACCTFILE/OUTFILE | Erro de I/O irrecuperável | ABEND imediato | Todos os parágrafos de I/O |

### Parágrafos de Tratamento de Erros

#### 9910-DISPLAY-IO-STATUS

**Linha 99100-99400:**
- **Trigger:** Invocado por qualquer erro de I/O antes do ABEND.
- **Ação:** Formata o IO-STATUS de 2 bytes para um formato de 4 bytes (NNNN) e exibe-o no SYSOUT.
- **Mensagens:** "FILE STATUS IS: NNNN [IO-STATUS-04]"
- **Terminação:** Retorna ao parágrafo chamador, que geralmente chamará o 9999-ABEND-PROGRAM.

### Rotina de ABEND

- **Parágrafo:** 9999-ABEND-PROGRAM (Linha 99990)
- **Condições:** Chamado quando um erro de I/O irrecuperável (Status diferente de 00 ou 10) ocorre durante OPEN, READ, WRITE ou CLOSE.
- **ABEND Code:** 999 (hardcoded)
- **Timing:** Immediate (com TIMING = 0)
- **Cleanup:** Nenhuma limpeza explícita é realizada além do fechamento de arquivos.

## 💼 REGRAS DE NEGÓCIO

### RN-001: Cópia Direta de Registro

**Descrição:** O programa deve ler o registro de contas de entrada e reescrevê-lo na íntegra no arquivo de saída, sem modificação de dados.

**Localização:** Linha 70300, Parágrafo 7000-PROCESS-RECORD

**Implementação:**

```cobol
MOVE WS-ACCT-FILE-REC TO OUTFILE-REC.
```

**Validações:**

Nenhuma validação de conteúdo é realizada.

**Exceções:**

O processamento é interrompido em caso de erro de I/O.

## 📈 CONSIDERAÇÕES DE PERFORMANCE

### Características de Performance

| Aspecto | Avaliação | Observação |
|---------|-----------|------------|
| Volume de Dados | Médio | Dependente do tamanho do ACCTFILE. |
| Tempo de Execução | Baixo | Programa simples de I/O Bound. |
| Uso de Memória | Baixo | Não possui grandes estruturas de dados ou tabelas. |
| I/O Intensity | Alto | O programa é essencialmente um processo de leitura/escrita 1:1. |
| CPU Intensity | Baixo | Mínimo de lógica de processamento (MOVE, incrementos de contador). |

### Pontos Críticos de Performance

Nenhum ponto de crítica de performance inerente, visto que a lógica é uma cópia sequencial direta, que é o tipo de processamento mais rápido no COBOL/Mainframe.

### Otimizações Aplicadas

Otimizado por ser um I/O sequencial simples. O uso de MOVE é a operação mais eficiente para retransmissão de dados.

### Gargalos Potenciais

**I/O Subsystem Speed:** A velocidade de execução é inteiramente determinada pela taxa de transferência de I/O dos datasets ACCTFILE e OUTFILE.

## 🔗 DEPENDÊNCIAS

### Mapa de Dependências

```
CBACT01C
│
├── Programas Externos
│     └── CEE3ABD (CALL na linha 99993)
│
├── Copybooks
│     ├── ACREC01C (linha 40100)
│     └── CBEMSG01 (linha 50100)
│
├── Arquivos
│     ├── ACCTFILE (DDName: ACCTFILE)
│     └── OUTFILE (DDName: OUTFILE)
│
└── Utilitários
      └── N/A
```

### Tabela de Dependências

| Tipo | Nome | Obrigatório | Observação |
|------|------|-------------|------------|
| Programa | CEE3ABD | SIM | Serviço de ABEND do IBM Language Environment (LE). |
| Copybook | ACREC01C | SIM | Layout do registro de contas. |
| Copybook | CBEMSG01 | SIM | Variáveis de controle de status. |
| Arquivo | ACCTFILE | SIM | Arquivo de entrada de contas. |
| Arquivo | OUTFILE | SIM | Arquivo de saída. |

## 🚀 GUIA DE EXECUÇÃO

### Pré-requisitos

**Arquivos Obrigatórios:**

- ACCTFILE - Arquivo de Contas Mestre (SEQUENTIAL).

**Programas/Módulos Necessários:**

- CBACT01C - Módulo de carga.
- CEE3ABD - Serviço de sistema (geralmente disponível automaticamente no ambiente LE).

**Recursos de Sistema:**

- Deve ter acesso de leitura ao DDName ACCTFILE.
- Deve ter acesso de escrita ao DDName OUTFILE.

### Parâmetros de JCL

**DDNames Obrigatórios:**

```jcl
//ACCTFILE DD DSN=[nome-dataset-entrada],
//            DISP=SHR
//OUTFILE  DD DSN=[nome-dataset-saida],
//            DISP=(NEW,CATLG,DELETE)
```

**Template JCL Completo:**

```jcl
//CBATC01C JOB (CARDDEMO),'ACCOUNT-COPY',
//          CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//*
//STEP01   EXEC PGM=CBACT01C
//STEPLIB  DD DSN=[load-library],DISP=SHR
//ACCTFILE DD DSN=[input-dataset.CBACT01C.INPUT],DISP=SHR
//OUTFILE  DD DSN=[output-dataset.CBACT01C.OUTPUT],
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=240,BLKSIZE=0)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
```

### Mensagens do Sistema

**Mensagens Informativas:**

| Mensagem | Momento | Significado |
|----------|---------|-------------|
| "ACCOUNT RECORDS READ: [NNNNNNNNN]" | Finalização | Total de registros lidos com sucesso. |
| "ACCOUNT RECORDS WRITTEN: [NNNNNNNNN]" | Finalização | Total de registros escritos com sucesso. |

**Mensagens de Erro:**

| Mensagem | Causa | Ação Corretiva |
|----------|-------|----------------|
| "ERROR OPENING ACCOUNT FILE" | Erro no OPEN do ACCTFILE (Status != 00). | Verificar status do arquivo e JCL. |
| "ERROR READING ACCOUNT FILE" | Erro de I/O não-EOF (Status != 00 ou 10). | Verificar status do arquivo, integridade do dataset. |
| "ERROR WRITING OUTPUT FILE" | Erro no WRITE do OUTFILE (Status != 00 ou 10). | Verificar espaço em disco e JCL. |
| "ABENDING PROGRAM" | Falha de I/O irrecuperável. | Verificar logs e JCL. |

### Códigos de Retorno

| Código | Significado | Ação |
|--------|-------------|------|
| 0000 | Sucesso | Nenhuma (Processamento concluído com sucesso e arquivos fechados). |
| 0008 | Erro no OPEN/CLOSE | Pode ser retornado em alguns casos de falha de I/O dependendo da configuração. |
| 0012 | Erro no OPEN/CLOSE | Pode ser retornado em alguns casos de falha de I/O dependendo da configuração. |
| 0999 | ABEND | Erro de I/O irrecuperável. |

## 🔧 MANUTENÇÃO E SUPORTE

### Pontos de Atenção para Manutenção

#### Áreas Críticas

**Rotinas de I/O e ABEND (Linhas 80000-99993)**

- **Criticidade:** Alta
- **Motivo:** Contém a lógica estrita de verificação de FILE STATUS e a chamada para ABEND. Qualquer alteração incorreta aqui pode resultar em loop infinito ou falhas de ABEND não diagnosticáveis.
- **Cuidados:** Preservar as checagens `IF ACCTFILE-STATUS = '00'` e o fluxo de erro (PERFORM 9910-DISPLAY-IO-STATUS, PERFORM 9999-ABEND-PROGRAM).

#### Lógica Complexa

Nenhuma lógica complexa explícita. O programa é uma simples rotina de cópia.

### Campos e Variáveis Críticos

| Campo | Tipo | Por que é Crítico | Impacto de Mudança |
|-------|------|-------------------|-------------------|
| ACCTFILE-STATUS | PIC XX | Controla todos os fluxos de erro. | Quebra do tratamento de erros/I/O. |
| END-OF-ACCTFILE | PIC X | Controla o loop principal. | Loop infinito ou processamento incompleto. |

### Cenários de Teste Recomendados

#### Teste 1: Processamento Padrão

**Objetivo:** Verificar a cópia correta de 100% dos registros.

**Dados de Entrada:**
- ACCTFILE com 100 registros válidos.

**Volume:** 100 registros.

**Resultado Esperado:**
- COUNT-ACCT-READ = 100.
- COUNT-ACCT-WRITE = 100.
- RC=00.

**Validações:**
- [X] Conteúdo de OUTFILE idêntico a ACCTFILE.
- [X] Número correto de registros nos contadores.

#### Teste 2: Arquivo Vazio (Edge Case)

**Objetivo:** Verificar o tratamento correto de EOF na primeira leitura.

**Dados de Entrada:**
- ACCTFILE com 0 registros (Arquivo vazio).

**Resultado Esperado:**
- COUNT-ACCT-READ = 0.
- COUNT-ACCT-WRITE = 0.
- RC=00.

**Validações:**
- [X] Programa termina corretamente sem ABEND.
- [X] Contadores são 0.

### Casos Extremos (Edge Cases)

| Caso | Descrição | Como é Tratado | Teste |
|------|-----------|----------------|-------|
| Erro de OPEN | Arquivo de entrada não encontrado (DDName ausente). | Programa ABENDa (RC=999) no parágrafo 9000-ACCTFILE-OPEN. | JCL sem DD statement para ACCTFILE. |
| Erro de WRITE | Espaço em disco insuficiente para OUTFILE. | Programa ABENDa (RC=999) no parágrafo 8000-OUTFILE-WRITE. | Teste de volume com limite de espaço no JCL. |

## 📖 GLOSSÁRIO

| Termo | Definição |
|-------|-----------|
| BATCH | Processamento em lote (não interativo). |
| DDName | Data Definition Name - Nome usado no JCL para referenciar um arquivo. |
| FILE STATUS | Código de 2 bytes que indica o resultado de uma operação de I/O em COBOL. |
| COMP-3 | Packed Decimal - Formato numérico compactado. |
| ABEND | Terminação Anormal (Erro irrecuperável de execução). |
| EOF | End of File - Fim do arquivo (Status COBOL '10'). |
| CEE3ABD | Serviço do IBM Language Environment para forçar o ABEND do programa. |

## 📎 APÊNDICES

### Apêndice A: Diagrama de Fluxo de Parágrafos

```
MAIN LOGIC
│
├─► 0000-PROGRAM-START
│     ├─► 1000-INITIALIZATION
│     │     ├─► 9000-ACCTFILE-OPEN
│     │     ├─► 9000-OUTFILE-OPEN
│     │     └─► 1100-ACCTFILE-READ (Primeira Leitura)
│     │
│     ├─► 2000-PROCESS-ACCTFILE
│     │     └─► 7000-PROCESS-RECORD (Loop UNTIL EOF)
│     │           ├─► 8000-OUTFILE-WRITE
│     │           └─► 1100-ACCTFILE-READ (Próximas Leituras)
│     │
│     └─► 9000-PROGRAM-END
│           ├─► 9000-ACCTFILE-CLOSE
│           └─► 9000-OUTFILE-CLOSE
│
└─► ERRO HANDLING (Chamado a partir de todos os parágrafos de I/O)
    ├─► 9910-DISPLAY-IO-STATUS
    └─► 9999-ABEND-PROGRAM (Chama CEE3ABD)
```

### Apêndice B: Layouts Completos de Registros

**Arquivo: ACCTFILE (Entrada) - via ACREC01C**

```
POSIÇÃO  CAMPO                    TIPO      TAM  FORMATO    DESCRIÇÃO
─────────────────────────────────────────────────────────────────────
001-016  ACREC-ACCT-ID           9(16)      16   DISPLAY    Número da conta (ou cartão)
017-020  ACREC-ACCT-STATUS       9(04)      04   DISPLAY    Status da conta
021-026  ACREC-ISSUE-DT          X(06)      06   DISPLAY    Data de emissão da conta (AAAAMMDD)
027-040  ACREC-CURR-BAL          S9(11)V99  14   COMP-3     Saldo atual
041-240  FILLER                  X(200)     200  DISPLAY    Preenchimento
─────────────────────────────────────────────────────────────────────
TOTAL: 240 bytes
```

**Arquivo: OUTFILE (Saída)**

```
POSIÇÃO  CAMPO                    TIPO      TAM  FORMATO    DESCRIÇÃO
─────────────────────────────────────────────────────────────────────
001-240  OUTFILE-REC             X(240)     240  DISPLAY    Cópia exata do registro de entrada
─────────────────────────────────────────────────────────────────────
TOTAL: 240 bytes
```

### Apêndice C: Mapeamento Completo de Transformações

```
ACCTFILE (Entrada)              OUTFILE (Saída)
┌─────────────────┐            ┌─────────────────┐
│ WS-ACCT-FILE-REC│───────────►│ OUTFILE-REC     │ (Cópia direta de 240 bytes)
└─────────────────┘            └─────────────────┘
```

### Apêndice D: Histórico de Versões

| Versão | Data | Autor | Mudanças |
|--------|------|-------|----------|
| v1.0 | 2025-11-15 | AWS / Documentador | Versão inicial, programa de cópia de arquivo 1:1. |

### Apêndice E: Referências e Documentação Relacionada

- Documentação do Copybook ACREC01C (Layout de registro de contas).
- Documentação do Copybook CBEMSG01 (Variáveis de status).
- IBM Language Environment Programming Guide para CEE3ABD.

---

## 📝 NOTAS FINAIS

**Data da Documentação:** 2025-11-15  
**Documentador:** Gemini (Modelo de Linguagem Grande)  
**Versão da Documentação:** 1.0  
**Status:** Aprovado

### Observações Gerais:

- Este programa é uma rotina de I/O puro, servindo primariamente para extração ou backup de dados, sem qualquer lógica de negócio complexa (como cálculos ou validações de domínio).

- O tratamento de erros de I/O é rigoroso, levando a um ABEND com código 999 em praticamente todos os status de erro irrecuperável.

### Próximas Revisões:

- Incluir a documentação completa dos copybooks ACREC01C e CBEMSG01 (se disponíveis).

- Adicionar estimativas de volume e tempo de execução reais após testes de produção.

---

**FIM DA DOCUMENTAÇÃO**