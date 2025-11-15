PROMPT PARA DOCUMENTAÇÃO TÉCNICA DO PROGRAMA CBACT01C.cbl

  # Instruções para Geração de Documentação Técnica

  Analise o código-fonte do programa COBOL CBACT01C.cbl e gere uma documentação técnica detalhada seguindo a estrutura abaixo:

  ## 1. VISÃO GERAL DO PROGRAMA
  - **Nome do Programa**: [Identificação completa]
  - **Tipo**: [Batch/Online/Transacional]
  - **Aplicação**: [Sistema a que pertence]
  - **Propósito Principal**: [Descrição da função principal em 2-3 linhas]
  - **Autor/Proprietário**: [Informações de autoria]
  - **Versão e Data**: [Extrair do rodapé do código]

  ## 2. ARQUITETURA E FLUXO DE DADOS

  ### 2.1 Diagrama de Fluxo
  Descreva o fluxo de dados em formato textual:
  [Arquivo de Entrada] → [Processamento] → [Arquivos de Saída]

  ### 2.2 Arquivos de Entrada
  Para cada arquivo de entrada, documente:
  - **Nome Lógico**:
  - **Nome Físico/DDName**:
  - **Organização**: [VSAM/Sequential/Indexed/etc.]
  - **Modo de Acesso**: [Sequential/Random/Dynamic]
  - **Chave(s)**: [Se aplicável]
  - **Tamanho do Registro**:
  - **Estrutura do Registro**: [Layout detalhado dos campos]
  - **Status do Arquivo**: [Variável de controle]

  ### 2.3 Arquivos de Saída
  Para cada arquivo de saída, documente:
  - **Nome Lógico**:
  - **Nome Físico/DDName**:
  - **Organização**:
  - **Modo de Gravação**:
  - **Tipo de Registro**: [Fixo/Variável]
  - **Tamanho do Registro**: [Mínimo e máximo se variável]
  - **Estrutura do Registro**: [Layout detalhado incluindo OCCURS, REDEFINES]
  - **Propósito**: [Explicar o que este arquivo contém]

  ## 3. ESTRUTURA DE DADOS

  ### 3.1 Copybooks Utilizados
  Liste e explique cada copybook:
  - **Nome do Copybook**:
    - **Propósito**:
    - **Principais Estruturas Definidas**:
    - **Campos Críticos**:

  ### 3.2 Working-Storage Section
  Documente as principais áreas de trabalho:
  - **Variáveis de Status de Arquivo**:
  - **Flags e Indicadores**:
  - **Áreas de Trabalho**:
  - **Constantes**:
  - **Campos Temporários**:

  ### 3.3 Estruturas de Dados Complexas
  Documente estruturas com:
  - Arrays (OCCURS)
  - Redefinições (REDEFINES)
  - Registros de tamanho variável
  - Campos COMP/COMP-3

  ## 4. LÓGICA DO PROGRAMA

  ### 4.1 Fluxo Principal (Procedure Division)
  Descreva o fluxo de execução passo a passo:

  **Inicialização:**
  1. [Passo 1]
  2. [Passo 2]
  ...

  **Processamento Principal:**
  1. [Loop principal]
  2. [Condições de parada]
  ...

  **Finalização:**
  1. [Fechamento de arquivos]
  2. [Mensagens finais]
  ...

  ### 4.2 Parágrafos e Seções
  Para cada parágrafo/seção principal, documente:

  #### [Nome do Parágrafo]
  - **Linha(s) no código**:
  - **Propósito**:
  - **Lógica**: [Descrição detalhada do que faz]
  - **Parâmetros**: [Se recebe via USING]
  - **Retorno**: [Se retorna valores]
  - **Chamado por**: [Quais parágrafos o invocam]
  - **Chama**: [Quais parágrafos ele invoca]
  - **Tratamento de Erros**: [Como trata erros]

  ## 5. OPERAÇÕES DE I/O

  ### 5.1 Operações de Leitura
  Documente:
  - Parágrafos responsáveis
  - Tipo de leitura (READ/READ NEXT)
  - Tratamento de EOF
  - Validações após leitura
  - Códigos de retorno esperados

  ### 5.2 Operações de Escrita
  Documente:
  - Parágrafos responsáveis
  - Tipo de escrita (WRITE/REWRITE)
  - Validações antes da escrita
  - Tratamento de erros
  - Códigos de retorno esperados

  ## 6. TRANSFORMAÇÕES DE DADOS

  ### 6.1 Conversões e Formatações
  Liste todas as transformações:
  - **Conversão de datas**: [Explicar formatos de entrada e saída]
  - **Cálculos**: [Fórmulas aplicadas]
  - **Movimentações de dados**: [MOVEs críticos]
  - **Defaults e valores fixos**: [Valores hardcoded]

  ### 6.2 Chamadas Externas
  Para cada CALL:
  - **Nome do Programa/Módulo**:
  - **Propósito**:
  - **Parâmetros Passados**:
  - **Valores Retornados**:
  - **Tratamento de Erros**:

  ## 7. TRATAMENTO DE ERROS

  ### 7.1 Códigos de Status
  Documente todos os códigos de status tratados:
  - **'00'**: [Significado]
  - **'10'**: [Significado]
  - **'9x'**: [Significado]
  - Outros códigos específicos

  ### 7.2 Rotinas de Erro
  - **Parágrafos de tratamento**:
  - **Mensagens de erro**:
  - **Ações tomadas** (ABEND, ROLLBACK, etc.):
  - **Códigos de terminação**:

  ## 8. REGRAS DE NEGÓCIO

  Liste todas as regras de negócio identificadas:
  1. [Regra 1]: [Descrição e localização no código]
  2. [Regra 2]: [Descrição e localização no código]
  ...

  Incluir:
  - Validações de dados
  - Cálculos de valores
  - Condições especiais
  - Defaults aplicados

  ## 9. PERFORMANCE E OTIMIZAÇÃO

  ### 9.1 Considerações de Performance
  - Volume de dados esperado
  - Frequência de execução
  - Recursos utilizados (memória, I/O)

  ### 9.2 Pontos de Atenção
  - Loops aninhados
  - Operações repetitivas
  - Acesso a arquivos grandes

  ## 10. DEPENDÊNCIAS

  ### 10.1 Programas Externos
  Liste programas chamados via CALL

  ### 10.2 Copybooks
  Liste todos os copybooks com COPY

  ### 10.3 Arquivos de Sistema
  - JCL associado
  - PROCs utilizadas
  - Datasets necessários

  ## 11. EXECUÇÃO E OPERAÇÃO

  ### 11.1 Pré-requisitos
  - Arquivos que devem existir
  - Parâmetros necessários
  - Recursos de sistema

  ### 11.2 Parâmetros de Execução
  - JCL padrão
  - DDNames obrigatórios
  - Parâmetros de runtime

  ### 11.3 Mensagens do Sistema
  Liste todas as mensagens DISPLAY:
  - Mensagens informativas
  - Mensagens de erro
  - Mensagens de depuração

  ### 11.4 Condições de Retorno
  - Return codes possíveis
  - Significado de cada código
  - Ações a tomar

  ## 12. MANUTENÇÃO E SUPORTE

  ### 12.1 Histórico de Alterações
  [Se disponível no código]

  ### 12.2 Pontos Críticos
  - Áreas sensíveis do código
  - Lógica complexa que requer atenção
  - Campos críticos

  ### 12.3 Testes Recomendados
  - Cenários de teste principais
  - Volumes de dados para teste
  - Validações importantes

  ## 13. GLOSSÁRIO
  Defina termos técnicos e acrônimos:
  - VSAM:
  - KSDS:
  - COMP-3:
  - etc.

  ## 14. APÊNDICES

  ### Apêndice A: Diagrama de Relacionamento de Parágrafos
  [Mapa visual/textual de chamadas entre parágrafos]

  ### Apêndice B: Layout Completo dos Registros
  [Tabelas com posição, nome, tipo, tamanho de cada campo]

  ### Apêndice C: Mapeamento Entrada-Saída
  [Tabela mostrando como campos de entrada são transformados em campos de saída]

  ### Apêndice D: Código de Exemplo de JCL
  [Exemplo de JCL para execução do programa]

  INSTRUÇÕES ADICIONAIS:

  1. Seja Específico: Use números de linha do código como referência (ex: "linha 165-198")
  2. Seja Técnico: Use terminologia COBOL correta
  3. Seja Completo: Não omita detalhes importantes mesmo que pareçam óbvios
  4. Seja Claro: Explique a lógica em linguagem natural além do código
  5. Inclua Exemplos: Quando possível, forneça exemplos de dados de entrada/saída
  6. Destaque Particularidades: Aponte usos especiais de COBOL (COMP-3, REDEFINES, OCCURS, etc.)
  7. Contextualize: Explique o "porquê" além do "como"
  8. Identifique Riscos: Aponte possíveis problemas ou limitações

  ---
