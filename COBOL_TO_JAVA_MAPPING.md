# Documentação de Mapeamento COBOL → Java
## Programa CBACT01C.cbl

---

## 📋 Índice
1. [Visão Geral](#visão-geral)
2. [Mapeamento de Tipos de Dados](#mapeamento-de-tipos-de-dados)
3. [Mapeamento de Estruturas](#mapeamento-de-estruturas)
4. [Mapeamento de Rotinas](#mapeamento-de-rotinas)
5. [Mapeamento de Operações I/O](#mapeamento-de-operações-io)
6. [Tratamento de Erros](#tratamento-de-erros)
7. [Lógica de Negócio](#lógica-de-negócio)

---

## 🎯 Visão Geral

### Programa COBOL Original
- **Nome**: CBACT01C.cbl
- **Tipo**: Batch COBOL Program
- **Linhas de código**: 431
- **Função**: Ler arquivo VSAM de contas e gerar 3 arquivos de saída

### Conversão Java
- **Classe Principal**: CBACT01C.java
- **Arquitetura**: Orientada a objetos com separação de responsabilidades
- **Padrão**: Service Layer Pattern

---

## 🔢 Mapeamento de Tipos de Dados

### Tipos Numéricos

| COBOL | Descrição | Java | Observações |
|-------|-----------|------|-------------|
| `PIC 9(11)` | Numérico 11 dígitos | `Long` | ID de conta |
| `PIC S9(10)V99` | Decimal com sinal, 10 int + 2 dec | `BigDecimal` | Valores monetários |
| `PIC S9(10)V99 COMP-3` | Packed decimal | `BigDecimal` | Packed decimal = BigDecimal em Java |
| `PIC S9(9) COMP` | Binary signed | `int` | Códigos de resultado |
| `PIC 9(4) BINARY` | Binary unsigned | `int` ou `short` | Valores pequenos |

### Tipos Alfanuméricos

| COBOL | Descrição | Java | Observações |
|-------|-----------|------|-------------|
| `PIC X(01)` | Char único | `String` | Status, flags |
| `PIC X(10)` | String 10 chars | `String` | Datas, IDs de grupo |
| `PIC X(04)` | String 4 chars | `String` | Filler, ano |
| `PIC X(289)` | String 289 chars | `String` ou `byte[]` | Dados brutos |

### Tipos Especiais

| COBOL | Java Equivalente | Arquivo Java |
|-------|------------------|--------------|
| `OCCURS 5 TIMES` | `List<BalanceEntry>` | ArrayAccountRecord.java |
| `REDEFINES` | Casting ou classes separadas | - |
| `88 level` (condition) | `boolean` methods ou enums | - |
| `RECORDING MODE V` | Classes com `getRecordLength()` | VariableRecord.java |

**Exemplo COBOL:**
```cobol
01  ARR-ACCT-BAL OCCURS 5 TIMES.
    05  ARR-ACCT-CURR-BAL        PIC S9(10)V99.
    05  ARR-ACCT-CURR-CYC-DEBIT  PIC S9(10)V99 COMP-3.
```

**Equivalente Java:**
```java
public class BalanceEntry {
    private BigDecimal currentBalance;      // PIC S9(10)V99
    private BigDecimal currentCycleDebit;   // PIC S9(10)V99 COMP-3
}
private List<BalanceEntry> balanceEntries; // OCCURS 5 TIMES
```

---

## 🏗️ Mapeamento de Estruturas

### 1. FILE SECTION (Arquivos)

| COBOL | Linhas | Java Equivalente | Arquivo |
|-------|--------|------------------|---------|
| `FD ACCTFILE-FILE` | 52-55 | `BufferedReader inputReader` | FileIOService.java |
| `FD OUT-FILE` | 56-69 | `BufferedWriter outputWriter` | FileIOService.java |
| `FD ARRY-FILE` | 71-78 | `BufferedWriter arrayWriter` | FileIOService.java |
| `FD VBRC-FILE` | 80-85 | `BufferedWriter variableWriter` | FileIOService.java |

### 2. WORKING-STORAGE SECTION (Variáveis)

| COBOL | Tipo | Java Equivalente |
|-------|------|------------------|
| `01 ACCTFILE-STATUS` | File status | `IOException` exceptions |
| `01 APPL-RESULT` | Result code | `int exitCode` |
| `01 END-OF-FILE` | Flag | `boolean endOfFile` |
| `01 ABCODE` | Abend code | `System.exit(code)` |

### 3. COPYBOOKS

| COBOL Copybook | Linhas | Java Equivalente | Arquivo |
|----------------|--------|------------------|---------|
| `COPY CVACT01Y` | 89 | `AccountRecord.java` | model/AccountRecord.java |
| `COPY CODATECN` | 90 | `DateFormatter.java` | service/DateFormatter.java |

---

## 🔄 Mapeamento de Rotinas (Paragraphs)

### Estrutura COBOL → Java

| Paragraph COBOL | Linhas | Método Java | Classe Java |
|-----------------|--------|-------------|-------------|
| **Main Flow** | 141-160 | `main()` + `processAllAccounts()` | CBACT01C.java + AccountProcessingService.java |
| `0000-ACCTFILE-OPEN` | 317-333 | `openFiles()` | FileIOService.java |
| `1000-ACCTFILE-GET-NEXT` | 165-198 | `readNextAccount()` + `processAccountRecord()` | FileIOService.java + AccountProcessingService.java |
| `1100-DISPLAY-ACCT-RECORD` | 200-213 | `displayAccountRecord()` | AccountTransformationService.java |
| `1300-POPUL-ACCT-RECORD` | 215-240 | `transformToOutputRecord()` | AccountTransformationService.java |
| `1350-WRITE-ACCT-RECORD` | 242-251 | `writeOutputRecord()` | FileIOService.java |
| `1400-POPUL-ARRAY-RECORD` | 253-261 | `transformToArrayRecord()` | AccountTransformationService.java |
| `1450-WRITE-ARRY-RECORD` | 263-274 | `writeArrayRecord()` | FileIOService.java |
| `1500-POPUL-VBRC-RECORD` | 276-285 | `transformToVBRecord1()` + `transformToVBRecord2()` | AccountTransformationService.java |
| `1550-WRITE-VB1-RECORD` | 287-300 | `writeVariableRecord1()` | FileIOService.java |
| `1575-WRITE-VB2-RECORD` | 302-315 | `writeVariableRecord2()` | FileIOService.java |
| `2000-OUTFILE-OPEN` | 334-350 | `openFiles()` | FileIOService.java |
| `3000-ARRFILE-OPEN` | 352-368 | `openFiles()` | FileIOService.java |
| `4000-VBRFILE-OPEN` | 370-386 | `openFiles()` | FileIOService.java |
| `9000-ACCTFILE-CLOSE` | 388-404 | `closeFiles()` | FileIOService.java |
| `9910-DISPLAY-IO-STATUS` | 413-426 | `printStackTrace()` + logging | Exception handling |
| `9999-ABEND-PROGRAM` | 406-410 | `System.exit(999)` | CBACT01C.java |

---

## 📂 Mapeamento de Operações I/O

### OPEN (Abertura de Arquivos)

**COBOL:**
```cobol
OPEN INPUT ACCTFILE-FILE
IF ACCTFILE-STATUS = '00'
    MOVE 0 TO APPL-RESULT
ELSE
    MOVE 12 TO APPL-RESULT
END-IF
```

**Java:**
```java
try {
    inputReader = new BufferedReader(new FileReader(inputFilePath));
    System.out.println("Arquivo de entrada aberto: " + inputFilePath);
} catch (IOException e) {
    System.err.println("Erro ao abrir arquivos: " + e.getMessage());
    throw e;
}
```

### READ (Leitura)

**COBOL:**
```cobol
READ ACCTFILE-FILE INTO ACCOUNT-RECORD.
IF ACCTFILE-STATUS = '00'
    MOVE 0 TO APPL-RESULT
ELSE
    IF ACCTFILE-STATUS = '10'
        MOVE 16 TO APPL-RESULT  *> EOF
    END-IF
END-IF
```

**Java:**
```java
String line = inputReader.readLine();
if (line == null) {
    return null; // EOF
}
return parseAccountRecord(line);
```

### WRITE (Escrita)

**COBOL:**
```cobol
WRITE OUT-ACCT-REC.
IF OUTFILE-STATUS NOT = '00' AND OUTFILE-STATUS NOT = '10'
    DISPLAY 'ACCOUNT FILE WRITE STATUS IS:' OUTFILE-STATUS
    PERFORM 9999-ABEND-PROGRAM
END-IF.
```

**Java:**
```java
try {
    outputWriter.write(record.toString());
    outputWriter.newLine();
} catch (IOException e) {
    throw new IOException("Erro ao escrever registro", e);
}
```

### CLOSE (Fechamento)

**COBOL:**
```cobol
CLOSE ACCTFILE-FILE
IF ACCTFILE-STATUS = '00'
    SUBTRACT APPL-RESULT FROM APPL-RESULT
ELSE
    ADD 12 TO ZERO GIVING APPL-RESULT
END-IF
```

**Java:**
```java
try {
    if (inputReader != null) {
        inputReader.close();
    }
} catch (IOException e) {
    System.err.println("Erro ao fechar arquivos: " + e.getMessage());
}
```

---

## ⚠️ Tratamento de Erros

### File Status Codes

| COBOL Status | Significado | Java Equivalente |
|--------------|-------------|------------------|
| `'00'` | Sucesso | Sem exceção |
| `'10'` | End of file | `readLine() == null` |
| `'9x'` | Erro de I/O | `IOException` |

### ABEND (Abnormal End)

**COBOL:**
```cobol
9999-ABEND-PROGRAM.
    DISPLAY 'ABENDING PROGRAM'
    MOVE 0 TO TIMING
    MOVE 999 TO ABCODE
    CALL 'CEE3ABD' USING ABCODE, TIMING.
```

**Java:**
```java
catch (Exception e) {
    System.err.println("ERRO FATAL NO PROCESSAMENTO");
    System.err.println("Mensagem: " + e.getMessage());
    e.printStackTrace();
    System.exit(999);
}
```

---

## 💼 Lógica de Negócio

### 1. Transformação de Data (Linhas 223-233)

**COBOL:**
```cobol
MOVE ACCT-REISSUE-DATE TO CODATECN-INP-DATE.
MOVE '2' TO CODATECN-TYPE.
MOVE '2' TO CODATECN-OUTTYPE.
CALL 'COBDATFT' USING CODATECN-REC.
MOVE CODATECN-0UT-DATE TO OUT-ACCT-REISSUE-DATE.
```

**Java:**
```java
String formattedReissueDate = DateFormatter.formatDate(source.getReissueDate());
output.setReissueDate(formattedReissueDate);
```

**Implementação:**
- Arquivo: `service/DateFormatter.java`
- Método: `formatDate(String inputDate)`
- Converte: `YYYYMMDD` → `YYYY-MM-DD`

### 2. Valor Padrão para Debit Zero (Linhas 236-238)

**COBOL:**
```cobol
IF ACCT-CURR-CYC-DEBIT EQUAL TO ZERO
    MOVE 2525.00 TO OUT-ACCT-CURR-CYC-DEBIT
END-IF.
```

**Java:**
```java
if (source.getCurrentCycleDebit() == null ||
    source.getCurrentCycleDebit().compareTo(BigDecimal.ZERO) == 0) {
    output.setCurrentCycleDebit(new BigDecimal("2525.00"));
} else {
    output.setCurrentCycleDebit(source.getCurrentCycleDebit());
}
```

**Localização:** `AccountTransformationService.java:52-57`

### 3. População de Array com Valores Fixos (Linhas 255-260)

**COBOL:**
```cobol
MOVE ACCT-CURR-BAL   TO ARR-ACCT-CURR-BAL(1).
MOVE 1005.00         TO ARR-ACCT-CURR-CYC-DEBIT(1).
MOVE ACCT-CURR-BAL   TO ARR-ACCT-CURR-BAL(2).
MOVE 1525.00         TO ARR-ACCT-CURR-CYC-DEBIT(2).
MOVE -1025.00        TO ARR-ACCT-CURR-BAL(3).
MOVE -2500.00        TO ARR-ACCT-CURR-CYC-DEBIT(3).
```

**Java:**
```java
arrayRecord.addBalanceEntry(source.getCurrentBalance(), new BigDecimal("1005.00"));
arrayRecord.addBalanceEntry(source.getCurrentBalance(), new BigDecimal("1525.00"));
arrayRecord.addBalanceEntry(new BigDecimal("-1025.00"), new BigDecimal("-2500.00"));
arrayRecord.addBalanceEntry(BigDecimal.ZERO, BigDecimal.ZERO);
arrayRecord.addBalanceEntry(BigDecimal.ZERO, BigDecimal.ZERO);
```

**Localização:** `AccountTransformationService.java:75-92`

### 4. Registros de Comprimento Variável (Linhas 288-315)

**COBOL:**
```cobol
MOVE 12 TO WS-RECD-LEN.
MOVE VBRC-REC1 TO VBR-REC(1:WS-RECD-LEN).
WRITE VBR-REC.
```

**Java:**
```java
int recordLength = record.getRecordLength(); // 12 ou 39
variableWriter.write(String.format("[%02d]%s", recordLength, record.toString()));
variableWriter.newLine();
```

**Localização:** `FileIOService.java:157-168`

---

## 📊 Estrutura de Classes Java

```
java-conversion/
├── CBACT01C.java (Classe principal com main)
├── model/
│   ├── AccountRecord.java              (ACCOUNT-RECORD)
│   ├── OutputAccountRecord.java        (OUT-ACCT-REC)
│   ├── ArrayAccountRecord.java         (ARR-ARRAY-REC)
│   └── VariableRecord.java             (VBRC-REC1/REC2)
└── service/
    ├── FileIOService.java              (Operações I/O)
    ├── AccountProcessingService.java   (Fluxo principal)
    ├── AccountTransformationService.java (Lógica de negócio)
    └── DateFormatter.java              (COBDATFT equivalente)
```

---

## 🔑 Principais Diferenças COBOL vs Java

| Aspecto | COBOL | Java |
|---------|-------|------|
| **Paradigma** | Procedural | Orientado a Objetos |
| **Estrutura** | Divisões e Paragraphs | Classes e Métodos |
| **Tipos** | Picture clauses | Classes wrapper e primitivos |
| **I/O** | File status codes | Exceptions |
| **Memória** | Alocação fixa | Garbage collection |
| **Arrays** | OCCURS com tamanho fixo | Collections dinâmicas |
| **Strings** | Tamanho fixo com espaços | Tamanho dinâmico |
| **Decimais** | COMP-3 (packed) | BigDecimal |
| **Erro** | File status + ABEND | Try-catch + exceptions |

---

## 📝 Notas de Implementação

### Decisões de Design

1. **BigDecimal para valores monetários**:
   - Evita problemas de arredondamento do `double`
   - Mantém precisão exata (COMP-3)

2. **Separação de responsabilidades**:
   - Model: Estruturas de dados
   - Service: Lógica de negócio e I/O
   - Main: Orquestração e controle

3. **Formato de arquivo de entrada**:
   - COBOL: Registro de tamanho fixo (300 bytes)
   - Java: CSV com pipe delimiter (mais simples para teste)

4. **Registros de comprimento variável**:
   - COBOL: RDW (Record Descriptor Word) nativo
   - Java: Prefixo `[NN]` com tamanho do registro

### Limitações Conhecidas

1. **Sem acesso direto a VSAM**:
   - COBOL: Acesso nativo a VSAM KSDS
   - Java: Arquivo sequencial (pode ser adaptado para banco de dados)

2. **Formato de data**:
   - COBOL: Chama rotina Assembler (COBDATFT)
   - Java: Implementação Java pura

3. **Valores hardcoded**:
   - Mantidos como no COBOL original (programa de demonstração)
   - Em produção, devem vir de configuração

---

## ✅ Checklist de Conversão

- [x] Estruturas de dados (FILE SECTION)
- [x] Variáveis de trabalho (WORKING-STORAGE)
- [x] Abertura de arquivos (OPEN)
- [x] Leitura sequencial (READ)
- [x] Escrita de registros (WRITE)
- [x] Fechamento de arquivos (CLOSE)
- [x] Transformação de dados (MOVE statements)
- [x] Lógica condicional (IF/ELSE)
- [x] Loops (PERFORM UNTIL)
- [x] Chamada externa (CALL COBDATFT)
- [x] Tratamento de erros (FILE-STATUS)
- [x] Arrays/Tabelas (OCCURS)
- [x] Registros variáveis (RECORDING MODE V)
- [x] Display de dados (DISPLAY)
- [x] Abend handling (CEE3ABD)

---

## 🚀 Como Executar

### Compilar
```bash
javac -d bin -sourcepath . CBACT01C.java
```

### Executar
```bash
java -cp bin com.aws.carddemo.CBACT01C \
    example-input.dat \
    output.dat \
    array.dat \
    variable.dat
```

### Verificar Saídas
```bash
cat output.dat
cat array.dat
cat variable.dat
```

---

## 📚 Referências

- COBOL original: `CBACT01C.cbl` (431 linhas)
- Copybooks: `CVACT01Y`, `CODATECN`
- IBM Enterprise COBOL Language Reference
- Java SE Documentation
- AWS Mainframe Modernization Documentation

---

**Versão:** 1.0
**Data:** 2025-01-15
**Autor:** Conversão automatizada COBOL → Java
