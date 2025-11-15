# INSTRUÇÕES PARA GERAÇÃO DE DOCUMENTAÇÃO TÉCNICA COBOL

  Analise o código-fonte COBOL fornecido e gere uma documentação técnica seguindo RIGOROSAMENTE o padrão de saída abaixo. Mantenha a estrutura, formatação e organização exatamente
   como especificado.

  ---

  # PADRÃO DE SAÍDA - DOCUMENTAÇÃO TÉCNICA

  ## 📋 IDENTIFICAÇÃO DO PROGRAMA

  | Atributo | Valor |
  |----------|-------|
  | **Nome do Programa** | [PROGRAM-ID] |
  | **Tipo** | [BATCH/ONLINE/CICS/IMS] |
  | **Aplicação** | [Nome da aplicação] |
  | **Autor** | [AUTHOR] |
  | **Versão** | [Extrair do código] |
  | **Data da Versão** | [Extrair do código] |
  | **Linguagem** | COBOL [Enterprise/II/85/etc.] |

  **Descrição Resumida:**
  [Descrição em 2-4 linhas do propósito do programa]

  **Função Principal:**
  [Explicar em uma frase a função principal]

  ---

  ## 📊 ARQUITETURA DO SISTEMA

  ### Diagrama de Fluxo de Dados

  ┌─────────────────┐
  │  ARQUIVOS DE    │
  │    ENTRADA      │
  └────────┬────────┘
           │
           ▼
  ┌─────────────────┐
  │   PROGRAMA      │
  │   [PROGRAM-ID]  │
  └────────┬────────┘
           │
           ▼
  ┌─────────────────┐
  │  ARQUIVOS DE    │
  │     SAÍDA       │
  └─────────────────┘

  ### Resumo de I/O

  | Tipo | Qtd | Nomes |
  |------|-----|-------|
  | **Entrada** | [N] | [Lista de DDNames] |
  | **Saída** | [N] | [Lista de DDNames] |
  | **Entrada/Saída** | [N] | [Lista de DDNames] |

  ---

  ## 📁 CATÁLOGO DE ARQUIVOS

  ### 🔵 ARQUIVOS DE ENTRADA

  #### [1] [NOME-LÓGICO-DO-ARQUIVO]

  | Propriedade | Especificação |
  |-------------|---------------|
  | **DDName** | [NOME-DD] |
  | **Organização** | [SEQUENTIAL/INDEXED/RELATIVE/VSAM] |
  | **Tipo VSAM** | [KSDS/ESDS/RRDS/N/A] |
  | **Modo de Acesso** | [SEQUENTIAL/RANDOM/DYNAMIC] |
  | **Record Key** | [Nome do campo chave] |
  | **File Status** | [Nome da variável] |
  | **Tamanho do Registro** | [NNN bytes] |

  **Layout do Registro:**

  | Pos | Campo | Tipo | Tamanho | Formato | Descrição |
  |-----|-------|------|---------|---------|-----------|
  | 001-011 | [NOME-CAMPO] | [9/X/S9] | [NN] | [COMP/COMP-3/DISPLAY] | [Descrição] |
  | [continuar...] |

  **Propósito:**
  [Explicar o que este arquivo contém e seu papel no processamento]

  ---

  ### 🟢 ARQUIVOS DE SAÍDA

  #### [1] [NOME-LÓGICO-DO-ARQUIVO]

  | Propriedade | Especificação |
  |-------------|---------------|
  | **DDName** | [NOME-DD] |
  | **Organização** | [SEQUENTIAL/INDEXED/RELATIVE/VSAM] |
  | **Modo de Acesso** | [SEQUENTIAL/RANDOM/DYNAMIC] |
  | **File Status** | [Nome da variável] |
  | **Tipo de Registro** | [FIXED/VARIABLE] |
  | **Tamanho do Registro** | [MIN-MAX ou FIXED bytes] |
  | **Recording Mode** | [F/V/U/N/A] |

  **Layout do Registro:**

  | Pos | Campo | Tipo | Tamanho | Formato | Descrição |
  |-----|-------|------|---------|---------|-----------|
  | 001-011 | [NOME-CAMPO] | [9/X/S9] | [NN] | [COMP/COMP-3/DISPLAY] | [Descrição] |
  | [continuar...] |

  **Propósito:**
  [Explicar o que este arquivo contém e seu papel no processamento]

  **Observações Especiais:**
  - [Listar características especiais como OCCURS, REDEFINES, DEPENDING ON, etc.]

  ---

  ## 📚 COPYBOOKS E INCLUDES

  ### [NOME-DO-COPYBOOK-1]

  | Propriedade | Valor |
  |-------------|-------|
  | **Nome** | [NOME] |
  | **Localização no Código** | Linha [NNN] |
  | **Tipo** | [Estrutura de Dados/Constantes/Máscaras/etc.] |

  **Estruturas Definidas:**
  - `[ESTRUTURA-1]` - [Descrição]
  - `[ESTRUTURA-2]` - [Descrição]

  **Campos Principais:**
  ```cobol
  [Copiar estrutura principal do copybook se disponível]

  Propósito:
  [Explicar para que serve este copybook]

  ---
  💾 WORKING-STORAGE SECTION

  Variáveis de Controle de Arquivo

  | Variável          | Tipo   | Uso                      |
  |-------------------|--------|--------------------------|
  | [FILE-STATUS-VAR] | PIC XX | Status do arquivo [NOME] |
  | [continuar...]    |        |                          |

  Flags e Indicadores

  | Variável       | Tipo  | Valores | Propósito   |
  |----------------|-------|---------|-------------|
  | [FLAG-NAME]    | PIC X | [Y/N]   | [Descrição] |
  | [continuar...] |       |         |             |

  Códigos de Retorno

  | Variável       | Valor | Condição   | Significado |
  |----------------|-------|------------|-------------|
  | [RETURN-CODE]  | [00]  | [88-LEVEL] | [Descrição] |
  | [continuar...] |       |            |             |

  Áreas de Trabalho

  | Área           | Estrutura   | Propósito   |
  |----------------|-------------|-------------|
  | [WS-AREA-NAME] | [Estrutura] | [Descrição] |
  | [continuar...] |             |             |

  Constantes e Literais

  | Nome           | Valor   | Uso         |
  |----------------|---------|-------------|
  | [CONST-NAME]   | [VALOR] | [Descrição] |
  | [continuar...] |         |             |

  ---
  🔄 FLUXO DE EXECUÇÃO

  Visão Geral do Fluxo

  INÍCIO
    │
    ├─► [0000-INICIALIZAÇÃO]
    │     ├─► Abrir arquivos
    │     ├─► Inicializar variáveis
    │     └─► Validar pré-condições
    │
    ├─► [1000-PROCESSAMENTO-PRINCIPAL]
    │     ├─► Loop de leitura
    │     ├─► Processamento de registros
    │     └─► Escrita de saídas
    │
    └─► [9000-FINALIZAÇÃO]
          ├─► Fechar arquivos
          ├─► Exibir estatísticas
          └─► Retornar código

  Sequência de Execução

  FASE 1: INICIALIZAÇÃO
  Linha [NNN-NNN]: [PARAGRAFO-NOME]
  │
  ├─ [Passo 1]: [Descrição]
  ├─ [Passo 2]: [Descrição]
  └─ [Passo N]: [Descrição]

  FASE 2: PROCESSAMENTO PRINCIPAL
  Linha [NNN-NNN]: [PARAGRAFO-NOME]
  │
  ├─ LOOP PRINCIPAL:
  │  ├─ Condição: [UNTIL/WHILE condição]
  │  ├─ Leitura: [Arquivo/Operação]
  │  ├─ Validação: [Checagens]
  │  ├─ Transformação: [Processamento]
  │  └─ Escrita: [Arquivo/Operação]
  │
  └─ Saída do Loop: [Condição de término]

  FASE 3: FINALIZAÇÃO
  Linha [NNN-NNN]: [PARAGRAFO-NOME]
  │
  ├─ [Passo 1]: [Descrição]
  ├─ [Passo 2]: [Descrição]
  └─ [Passo N]: [Descrição]

  ---
  📝 CATÁLOGO DE PARÁGRAFOS

  [XXXX-NOME-DO-PARAGRAFO]

  | Propriedade | Detalhe                                                      |
  |-------------|--------------------------------------------------------------|
  | Localização | Linhas [NNN-NNN]                                             |
  | Tipo        | [Inicialização/Processamento/I-O/Validação/Erro/Finalização] |
  | Propósito   | [Descrição concisa]                                          |

  Lógica Detalhada:
  1. [Passo 1 com descrição]
  2. [Passo 2 com descrição]
  3. [Passo N com descrição]

  Parágrafos Invocados:
  - [PARAGRAFO-1] (linha [NNN])
  - [PARAGRAFO-2] (linha [NNN])

  Invocado Por:
  - [PARAGRAFO-X] (linha [NNN])
  - [PARAGRAFO-Y] (linha [NNN])

  Tratamento de Erros:
  - [Descrição de como erros são tratados]
  - Códigos de status verificados: [códigos]
  - Ação em caso de erro: [Ação]

  Variáveis Modificadas:
  - [VAR-1] - [Como é modificada]
  - [VAR-2] - [Como é modificada]

  Observações:
  - [Notas importantes sobre este parágrafo]

  ---
  🔀 OPERAÇÕES DE I/O

  Operações de Leitura

  | Arquivo   | Parágrafo             | Tipo             | Tratamento EOF | Status Verificados |
  |-----------|-----------------------|------------------|----------------|--------------------|
  | [ARQUIVO] | [PARA-XXXX] (L:[NNN]) | [READ/READ NEXT] | [Ação]         | [00, 10, outros]   |

  Lógica de Leitura [ARQUIVO]:
  1. READ [arquivo] INTO [área]
  2. IF status = '00' THEN
       - [Ação sucesso]
  3. ELSE IF status = '10' THEN
       - [Ação EOF]
  4. ELSE
       - [Ação erro]

  Operações de Escrita

  | Arquivo   | Parágrafo             | Tipo            | Validação Prévia      | Status Verificados |
  |-----------|-----------------------|-----------------|-----------------------|--------------------|
  | [ARQUIVO] | [PARA-XXXX] (L:[NNN]) | [WRITE/REWRITE] | [Sim/Não - Descrição] | [00, 10, outros]   |

  Lógica de Escrita [ARQUIVO]:
  1. [Preparação dos dados]
  2. WRITE [registro]
  3. IF status NOT = '00' AND NOT = '10' THEN
       - [Ação erro]

  ---
  🔧 TRANSFORMAÇÕES E PROCESSAMENTO

  Mapeamento Entrada → Saída

  Registro [ENTRADA] → Registro [SAÍDA]:

  | Campo Origem   | Transformação                 | Campo Destino | Regra                |
  |----------------|-------------------------------|---------------|----------------------|
  | [CAMPO-IN]     | [Direto/Convertido/Calculado] | [CAMPO-OUT]   | [Descrição da regra] |
  | [continuar...] |                               |               |                      |

  Cálculos e Fórmulas

  [Nome do Cálculo]

  Linha [NNN]:
  Fórmula: [Descrição matemática]
  Implementação: [Código COBOL]
  Propósito: [Explicação]

  Conversões de Formato

  | Tipo           | Campo   | De → Para               | Método         | Localização |
  |----------------|---------|-------------------------|----------------|-------------|
  | Data           | [CAMPO] | [FORMATO1] → [FORMATO2] | [CALL/Função]  | Linha [NNN] |
  | Numérico       | [CAMPO] | [FORMATO1] → [FORMATO2] | [MOVE/COMPUTE] | Linha [NNN] |
  | [continuar...] |         |                         |                |             |

  Valores Padrão (Default Values)

  | Campo          | Valor Padrão | Condição          | Linha |
  |----------------|--------------|-------------------|-------|
  | [CAMPO]        | [VALOR]      | [Quando aplicado] | [NNN] |
  | [continuar...] |              |                   |       |

  ---
  📞 CHAMADAS EXTERNAS

  [NOME-PROGRAMA-1]

  | Propriedade | Detalhe                           |
  |-------------|-----------------------------------|
  | Nome        | [PROGRAM-NAME]                    |
  | Tipo        | [Programa COBOL/Assembler/C/etc.] |
  | Localização | Linha [NNN]                       |
  | Invocado em | Parágrafo [XXXX-NOME]             |

  Parâmetros Passados:

  | Ordem | Nome      | Tipo     | Direção             | Propósito   |
  |-------|-----------|----------|---------------------|-------------|
  | 1     | [PARAM-1] | [PIC...] | [INPUT/OUTPUT/BOTH] | [Descrição] |
  | 2     | [PARAM-2] | [PIC...] | [INPUT/OUTPUT/BOTH] | [Descrição] |

  Retorno:
  - [Descrição do que é retornado]
  - Valores possíveis: [Lista]

  Tratamento de Erros:
  - [Como erros são detectados e tratados]

  Propósito da Chamada:
  [Explicar por que este programa é chamado]

  ---
  ⚠️ TRATAMENTO DE ERROS E EXCEÇÕES

  Matriz de Códigos de Status

  | Código      | Origem    | Significado | Ação Tomada | Parágrafo   |
  |-------------|-----------|-------------|-------------|-------------|
  | 00          | [Arquivo] | Sucesso     | Continuar   | [PARA-XXXX] |
  | 10          | [Arquivo] | EOF         | [Ação]      | [PARA-XXXX] |
  | 9x          | [Arquivo] | Erro VSAM   | ABEND       | [PARA-9999] |
  | [outros...] |           |             |             |             |

  Parágrafos de Tratamento de Erros

  [9XXX-NOME-ERRO]

  Linha [NNN-NNN]:
  Trigger: [O que causa a execução deste parágrafo]
  Ação: [O que o parágrafo faz]
  Mensagens: [Mensagens exibidas]
  Terminação: [Como termina - ABEND/RETURN CODE/etc.]

  Mensagens de Erro

  | Mensagem           | Severidade           | Causa   | Ação Requerida |
  |--------------------|----------------------|---------|----------------|
  | "[MENSAGEM EXATA]" | [INFO/WARNING/ERROR] | [Causa] | [Ação]         |
  | [continuar...]     |                      |         |                |

  Rotina de ABEND

  Parágrafo: [NOME] (Linha [NNN])
  Condições: [Quando é chamado]
  ABEND Code: [NNN]
  Timing: [Immediate/Delayed]
  Cleanup: [Sim/Não - O que é feito antes]

  ---
  💼 REGRAS DE NEGÓCIO

  RN-001: [Nome da Regra]

  Descrição: [Explicação da regra de negócio]
  Localização: Linha(s) [NNN], Parágrafo [XXXX-NOME]
  Implementação:
    - [Passo 1 da implementação]
    - [Passo 2 da implementação]
  Validações:
    - [Validação 1]
    - [Validação 2]
  Exceções:
    - [Exceção 1]
    - [Exceção 2]

  RN-002: [Nome da Regra]

  [Mesmo formato acima]

  ---
  📈 CONSIDERAÇÕES DE PERFORMANCE

  Características de Performance

  | Aspecto           | Avaliação          | Observação                |
  |-------------------|--------------------|---------------------------|
  | Volume de Dados   | [Alto/Médio/Baixo] | [Estimativa]              |
  | Tempo de Execução | [HH:MM:SS]         | [Em condições normais]    |
  | Uso de Memória    | [Alto/Médio/Baixo] | [Estimativa]              |
  | I/O Intensity     | [Alto/Médio/Baixo] | [Leituras/Escritas]       |
  | CPU Intensity     | [Alto/Médio/Baixo] | [Cálculos/Transformações] |

  Pontos Críticos de Performance

  [Ponto Crítico 1]

  Localização: [Linha/Parágrafo]
  Descrição: [O que pode causar lentidão]
  Impacto: [Alto/Médio/Baixo]
  Recomendação: [Sugestão de melhoria]

  Otimizações Aplicadas


  Gargalos Potenciais

  - [Gargalo 1]: [Descrição e como mitigar]
  - [Gargalo 2]: [Descrição e como mitigar]

  ---
  🔗 DEPENDÊNCIAS

  Mapa de Dependências

  [PROGRAM-ID]
    │
    ├── Programas Externos
    │     ├── [PROG-1] (CALL na linha [NNN])
    │     └── [PROG-2] (CALL na linha [NNN])
    │
    ├── Copybooks
    │     ├── [COPY-1] (linha [NNN])
    │     └── [COPY-2] (linha [NNN])
    │
    ├── Arquivos
    │     ├── [FILE-1] (DDName: [DD])
    │     └── [FILE-2] (DDName: [DD])
    │
    └── Utilitários
          ├── [UTIL-1] (Tipo: [Assembly/System])
          └── [UTIL-2] (Tipo: [Assembly/System])

  Tabela de Dependências

  | Tipo           | Nome   | Obrigatório | Observação |
  |----------------|--------|-------------|------------|
  | Programa       | [NOME] | [SIM/NÃO]   | [Nota]     |
  | Copybook       | [NOME] | [SIM/NÃO]   | [Nota]     |
  | Arquivo        | [NOME] | [SIM/NÃO]   | [Nota]     |
  | [continuar...] |        |             |            |

  ---
  🚀 GUIA DE EXECUÇÃO

  Pré-requisitos

  Arquivos Obrigatórios:
  - [ARQUIVO-1] - [Descrição/Conteúdo]
  - [ARQUIVO-2] - [Descrição/Conteúdo]

  Programas/Módulos Necessários:
  - [PROGRAMA-1] - [Disponível em...]
  - [PROGRAMA-2] - [Disponível em...]

  Recursos de Sistema:
  - Region Size: [NNNNk]
  - Tempo Máximo: [HH:MM]
  - Espaço em Disco: [NNNG]

  Parâmetros de JCL

  DDNames Obrigatórios:

  //DDNAME1  DD DSN=[nome-dataset],
  //            DISP=[disposição]
  //DDNAME2  DD DSN=[nome-dataset],
  //            DISP=[disposição]
  [continuar...]

  Template JCL Completo:

  //[JOBNAME] JOB ([accounting]),'[description]',
  //          CLASS=[X],MSGCLASS=[X],NOTIFY=&SYSUID
  //*
  //STEP01   EXEC PGM=[PROGRAM-ID]
  //STEPLIB  DD DSN=[load-library],DISP=SHR
  //ACCTFILE DD DSN=[input-dataset],DISP=SHR
  //OUTFILE  DD DSN=[output-dataset],
  //            DISP=(NEW,CATLG,DELETE),
  //            SPACE=(CYL,(nn,nn),RLSE),
  //            DCB=(RECFM=FB,LRECL=nnn,BLKSIZE=nnnnn)
  [continuar para todos os DD statements...]
  //SYSOUT   DD SYSOUT=*

  Mensagens do Sistema

  Mensagens Informativas:
  | Mensagem     | Momento              | Significado   |
  |--------------|----------------------|---------------|
  | "[MENSAGEM]" | [Início/Durante/Fim] | [Significado] |

  Mensagens de Erro:
  | Mensagem     | Causa   | Ação Corretiva |
  |--------------|---------|----------------|
  | "[MENSAGEM]" | [Causa] | [Ação]         |

  Códigos de Retorno

  | Código | Significado | Ação    |
  |--------|-------------|---------|
  | 0000   | Sucesso     | Nenhuma |
  | 0004   | [Descrição] | [Ação]  |
  | 0008   | [Descrição] | [Ação]  |
  | 0012   | [Descrição] | [Ação]  |
  | 0999   | ABEND       | [Ação]  |

  Estatísticas de Execução

  Contadores Exibidos:

  Validação de Sucesso:
  ✓ [Verificação 1]
  ✓ [Verificação 2]
  ✓ [Verificação N]

  ---
  🔧 MANUTENÇÃO E SUPORTE

  Pontos de Atenção para Manutenção

  Áreas Críticas

  1. [Área 1] (Linhas [NNN-NNN])
    - Criticidade: [Alta/Média/Baixa]
    - Motivo: [Por que é crítica]
    - Cuidados: [O que observar ao modificar]
  2. [Área 2] (Linhas [NNN-NNN])
    - [Mesmo formato]

  Lógica Complexa

  [Descrição da Complexidade]:
  Localização: Linhas [NNN-NNN], Parágrafo [XXXX]
  Complexidade: [Alta/Média]
  Descrição: [O que torna complexo]
  Documentação Adicional: [Referências]
  Recomendação: [Como abordar mudanças]

  Campos e Variáveis Críticos

  | Campo          | Tipo   | Por que é Crítico | Impacto de Mudança |
  |----------------|--------|-------------------|--------------------|
  | [CAMPO]        | [Tipo] | [Razão]           | [Impacto]          |
  | [continuar...] |        |                   |                    |

  Cenários de Teste Recomendados

  Teste 1: [Nome do Cenário]

  Objetivo: [O que testar]
  Dados de Entrada:
    - [Descrição dos dados]
    - Volume: [quantidade de registros]
  Resultado Esperado:
    - [Resultado 1]
    - [Resultado 2]
  Validações:
    - [ ] [Validação 1]
    - [ ] [Validação 2]

  Teste 2: [Nome do Cenário]

  [Mesmo formato]

  Casos Extremos (Edge Cases)

  | Caso           | Descrição   | Como é Tratado | Teste         |
  |----------------|-------------|----------------|---------------|
  | [Caso 1]       | [Descrição] | [Tratamento]   | [Como testar] |
  | [continuar...] |             |                |               |

  ---
  📖 GLOSSÁRIO

  | Termo     | Definição                                                |
  |-----------|----------------------------------------------------------|
  | VSAM      | Virtual Storage Access Method - Método de acesso...      |
  | KSDS      | Key Sequenced Data Set - Dataset sequencial por chave... |
  | COMP-3    | Packed Decimal - Formato de armazenamento...             |
  | [Termo N] | [Definição]                                              |

  ---
  📎 APÊNDICES

  Apêndice A: Diagrama de Fluxo de Parágrafos

  MAIN LOGIC
      │
      ├─► 0000-INIT
      │     ├─► 0100-OPEN-FILES
      │     │     ├─► 0110-OPEN-INPUT
      │     │     └─► 0120-OPEN-OUTPUT
      │     └─► 0200-INITIALIZE-VARS
      │
      ├─► 1000-PROCESS
      │     ├─► 1100-READ-INPUT
      │     ├─► 1200-VALIDATE
      │     ├─► 1300-TRANSFORM
      │     └─► 1400-WRITE-OUTPUT
      │
      └─► 9000-FINALIZE
            ├─► 9100-CLOSE-FILES
            └─► 9200-DISPLAY-STATS

  ERRO HANDLING
      │
      ├─► 9900-ERROR-HANDLER
      ├─► 9910-DISPLAY-STATUS
      └─► 9999-ABEND

  Apêndice B: Layouts Completos de Registros

  Arquivo: [NOME-ARQUIVO]

  POSIÇÃO  CAMPO                    TIPO      TAM  FORMATO    DESCRIÇÃO
  ─────────────────────────────────────────────────────────────────────
  001-011  [CAMPO-1]               9(11)      11   DISPLAY    [Desc]
  012-012  [CAMPO-2]               X          01   DISPLAY    [Desc]
  013-025  [CAMPO-3]               S9(10)V99  13   COMP-3     [Desc]
  [continuar...]
  ─────────────────────────────────────────────────────────────────────
  TOTAL: [NNN] bytes

  Apêndice C: Mapeamento Completo de Transformações

  ACCTFILE (Entrada)              OUTFILE (Saída)
  ┌─────────────────┐            ┌─────────────────┐
  │ FD-ACCT-ID      │───────────►│ OUT-ACCT-ID     │ (Direto)
  │ ACCT-CURR-BAL   │───────────►│ OUT-CURR-BAL    │ (Direto)
  │ ACCT-REISSUE-DT │──┐         │ OUT-REISSUE-DT  │
  └─────────────────┘  │         └─────────────────┘
                       │
                       └─► CALL COBDATFT (Conversão de Data)

  Apêndice D: Histórico de Versões

  | Versão                            | Data         | Autor   | Mudanças    |
  |-----------------------------------|--------------|---------|-------------|
  | [v1.0]                            | [YYYY-MM-DD] | [Autor] | [Descrição] |
  | [Extrair do código se disponível] |              |         |             |

  Apêndice E: Referências e Documentação Relacionada


  ---
  📝 NOTAS FINAIS

  Data da Documentação: [YYYY-MM-DD]
  Documentador: [Nome/Sistema]
  Versão da Documentação: [N.N]
  Status: [Revisão/Aprovado/Rascunho]

  Observações Gerais:
  - [Nota importante 1]
  - [Nota importante 2]

  Próximas Revisões:
  - [Item a revisar 1]
  - [Item a revisar 2]

  ---
  FIM DA DOCUMENTAÇÃO


  ---

  ## INSTRUÇÕES DE PREENCHIMENTO:

  1. **Formatação**:
     - Mantenha EXATAMENTE a estrutura de markdown
     - Use os símbolos especificados (📋, 📊, 📁, etc.)
     - Preserve todas as tabelas no formato indicado
     - Use blocos de código com ```quando especificado

  2. **Completude**:
     - Preencha TODAS as seções, mesmo que seja para indicar "N/A"
     - Não omita seções vazias, indique "Nenhum" ou "Não aplicável"
     - Numere todas as regras de negócio (RN-001, RN-002, etc.)

  3. **Referências**:
     - SEMPRE inclua números de linha para referências
     - Use o formato: "Linha [NNN]" ou "Linhas [NNN-NNN]"
     - Faça cross-reference entre seções

  4. **Nível de Detalhe**:
     - Seja específico e técnico
     - Use terminologia COBOL correta
     - Inclua exemplos quando necessário
     - Explique o "porquê" além do "como"

  5. **Padronização**:
     - Nomes de variáveis em `código-fonte`
     - Parágrafos em **negrito**
     - Seções com emojis conforme template
     - Tabelas sempre com headers em **negrito**

  6. **Reutilização**:
     - Este template DEVE ser aplicável a qualquer programa COBOL
     - Mantenha a estrutura mesmo se algumas seções forem "Nenhum"
     - Preserve a ordem das seções

  ---

  **ESTE É O PADRÃO OBRIGATÓRIO DE SAÍDA. NÃO DESVIE DESTA ESTRUTURA.**
