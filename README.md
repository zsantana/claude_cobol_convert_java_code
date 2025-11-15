# CBACT01C - Conversão COBOL para Java

Conversão do programa COBOL batch **CBACT01C.cbl** para Java.

## 📋 Descrição

Este projeto é uma conversão completa do programa COBOL CBACT01C.cbl para Java, mantendo toda a funcionalidade original:

- Leitura de arquivo de contas (ACCTFILE)
- Geração de 3 arquivos de saída com diferentes formatos
- Transformação de dados com lógica de negócio
- Tratamento robusto de erros

## 🗂️ Estrutura do Projeto

```
java-conversion/
├── CBACT01C.java                         # Classe principal
├── model/                                # Modelos de dados (DTOs)
│   ├── AccountRecord.java
│   ├── OutputAccountRecord.java
│   ├── ArrayAccountRecord.java
│   └── VariableRecord.java
├── service/                              # Serviços e lógica de negócio
│   ├── FileIOService.java
│   ├── AccountProcessingService.java
│   ├── AccountTransformationService.java
│   └── DateFormatter.java
├── example-input.dat                     # Arquivo de exemplo
├── COBOL_TO_JAVA_MAPPING.md              # Documentação detalhada
└── README.md                             # Este arquivo
```

## 🔄 Mapeamento COBOL → Java

### Classes de Modelo

| COBOL Structure | Java Class | Descrição |
|-----------------|------------|-----------|
| `FD-ACCTFILE-REC` | `AccountRecord.java` | Registro de entrada |
| `OUT-ACCT-REC` | `OutputAccountRecord.java` | Registro de saída formatado |
| `ARR-ARRAY-REC` | `ArrayAccountRecord.java` | Registro com array (OCCURS) |
| `VBRC-REC1/REC2` | `VariableRecord.java` | Registros de comprimento variável |

### Tipos de Dados

| COBOL | Java |
|-------|------|
| `PIC 9(11)` | `Long` |
| `PIC S9(10)V99` | `BigDecimal` |
| `PIC S9(10)V99 COMP-3` | `BigDecimal` |
| `PIC X(n)` | `String` |
| `OCCURS n TIMES` | `List<T>` |

### Rotinas Principais

| COBOL Paragraph | Java Method | Classe |
|-----------------|-------------|--------|
| `1000-ACCTFILE-GET-NEXT` | `readNextAccount()` | `FileIOService` |
| `1300-POPUL-ACCT-RECORD` | `transformToOutputRecord()` | `AccountTransformationService` |
| `1400-POPUL-ARRAY-RECORD` | `transformToArrayRecord()` | `AccountTransformationService` |
| `CALL 'COBDATFT'` | `DateFormatter.formatDate()` | `DateFormatter` |

## 🚀 Como Usar

### Pré-requisitos

- Java 8 ou superior
- JDK instalado

### Compilação

```bash
# Navegar até o diretório java-conversion
cd java-conversion

# Compilar todos os arquivos Java
javac -d bin -sourcepath . CBACT01C.java model/*.java service/*.java
```

### Execução

```bash
# Executar o programa
java -cp bin com.aws.carddemo.CBACT01C \
    example-input.dat \
    output.dat \
    array.dat \
    variable.dat
```

### Parâmetros

1. **Arquivo de entrada** - Arquivo com dados de contas (formato CSV com pipe)
2. **Arquivo de saída 1** - Registros formatados (OUTFILE)
3. **Arquivo de saída 2** - Registros com arrays (ARRYFILE)
4. **Arquivo de saída 3** - Registros variáveis (VBRCFILE)

## 📄 Formato do Arquivo de Entrada

O arquivo de entrada deve estar no formato CSV com pipe (`|`) como delimitador:

```
ACCT_ID|STATUS|CURR_BAL|CREDIT_LIM|CASH_LIM|OPEN_DT|EXP_DT|REISS_DT|CYC_CRED|CYC_DEB|GRP_ID
```

**Exemplo:**
```
00000000001|Y|5000.00|10000.00|2000.00|2024-01-15|2027-01-15|20250115|1500.00|0.00|GRP0000001
00000000002|Y|7500.50|15000.00|3000.00|2024-02-20|2027-02-20|20250220|2000.00|500.00|GRP0000001
```

Veja `example-input.dat` para exemplo completo.

## 📊 Arquivos de Saída

### 1. OUTFILE (output.dat)
Registro formatado com todos os campos da conta, incluindo data reformatada.

### 2. ARRYFILE (array.dat)
Registro com array de 5 ocorrências de balances, demonstrando uso de tabelas.

### 3. VBRCFILE (variable.dat)
Dois tipos de registros de comprimento variável:
- Tipo 1: 12 bytes (ID + Status)
- Tipo 2: 39 bytes (ID + Balance + Limit + Year)

## 🔍 Lógica de Negócio

### 1. Formatação de Data
- Converte data de `YYYYMMDD` para `YYYY-MM-DD`
- Equivalente à chamada `CALL 'COBDATFT'` do COBOL

### 2. Valor Padrão para Debit Zero
Se `currentCycleDebit = 0`, atribui valor `2525.00`

### 3. População de Array
Cria array com 5 ocorrências, incluindo valores negativos para teste de formatos numéricos.

### 4. Registros Variáveis
Gera dois tipos de registros com tamanhos diferentes (12 e 39 bytes).

## ⚠️ Tratamento de Erros

O programa replica o tratamento de erros do COBOL original:

- **Arquivo não encontrado**: Exit code 8
- **Erro de I/O**: Exception com mensagem detalhada
- **Erro fatal**: Exit code 999 (equivalente ao ABEND do COBOL)

## 🧪 Testando

### Teste Básico

```bash
# Compilar
javac -d bin -sourcepath . CBACT01C.java model/*.java service/*.java

# Executar com arquivo de exemplo
java -cp bin com.aws.carddemo.CBACT01C \
    example-input.dat \
    test-output.dat \
    test-array.dat \
    test-variable.dat

# Verificar saídas
cat test-output.dat
cat test-array.dat
cat test-variable.dat
```

### Saída Esperada

```
╔════════════════════════════════════════════════════════╗
║  CBACT01C - Account File Processing Program           ║
║  Java v1.0 (Converted from COBOL)                     ║
╚════════════════════════════════════════════════════════╝

START OF EXECUTION OF PROGRAM CBACT01C
===========================================
Arquivo de entrada aberto: example-input.dat
Arquivo de saída aberto: test-output.dat
Arquivo de array aberto: test-array.dat
Arquivo de registro variável aberto: test-variable.dat

ACCT-ID                 : 1
ACCT-ACTIVE-STATUS      : Y
ACCT-CURR-BAL           : 5000.00
...
===========================================
END OF EXECUTION OF PROGRAM CBACT01C
Total de registros processados: 5
Total de registros com erro: 0

Programa finalizado com código de retorno: 0
```

## 📚 Documentação Adicional

Para documentação técnica detalhada sobre o mapeamento COBOL → Java, consulte:

- **[COBOL_TO_JAVA_MAPPING.md](COBOL_TO_JAVA_MAPPING.md)** - Mapeamento completo de tipos, estruturas e rotinas

## 🔧 Customização

### Alterando Formato de Entrada

Para usar formato diferente de CSV, modifique o método `parseAccountRecord()` em `FileIOService.java`.

### Usando Banco de Dados

Para substituir arquivos por banco de dados:

1. Adicione dependência JDBC
2. Modifie `FileIOService` para usar `Connection` e `PreparedStatement`
3. Implemente `readNextAccount()` com `ResultSet`

### Adicionando Logging

Para adicionar logging profissional:

1. Adicione Log4j ou SLF4J
2. Substitua `System.out.println()` por `logger.info()`
3. Configure níveis de log (INFO, DEBUG, ERROR)

## 🐛 Problemas Conhecidos

1. **Formato de arquivo**: Usa CSV em vez de registro de tamanho fixo do COBOL
2. **VSAM**: Não suporta acesso direto a arquivos VSAM (pode ser adaptado para SQL)
3. **Valores hardcoded**: Mantidos como no COBOL original (programa de demonstração)

## 📝 Notas de Versão

### v1.0 (2025-01-15)
- ✅ Conversão inicial completa
- ✅ Todos os tipos de dados mapeados
- ✅ Todas as rotinas implementadas
- ✅ Lógica de negócio preservada
- ✅ Tratamento de erros funcional
- ✅ Documentação completa

## 👥 Contribuindo

Para melhorias ou correções:

1. Fork o projeto
2. Crie uma branch para sua feature
3. Commit suas mudanças
4. Push para a branch
5. Abra um Pull Request

## 📄 Licença

Apache License 2.0 (mesma do programa COBOL original)

---

**Programa COBOL Original**: CBACT01C.cbl (431 linhas)
**Conversão Java**: 10 arquivos, ~1500 linhas
**Status**: ✅ Funcional e testado
