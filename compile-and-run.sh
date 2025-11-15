#!/bin/bash

###############################################################################
# Script para compilar e executar o programa CBACT01C Java
# Conversão do programa COBOL CBACT01C.cbl
###############################################################################

# Cores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Diretórios
BIN_DIR="bin"
SRC_DIR="."

# Arquivos de teste
INPUT_FILE="example-input.dat"
OUTPUT_FILE="output.dat"
ARRAY_FILE="array.dat"
VARIABLE_FILE="variable.dat"

echo -e "${BLUE}╔════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║  CBACT01C - Compilação e Execução                     ║${NC}"
echo -e "${BLUE}║  Conversão COBOL → Java                                ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════╝${NC}"
echo ""

# Limpar compilação anterior
echo -e "${YELLOW}[1/4] Limpando compilação anterior...${NC}"
rm -rf $BIN_DIR
rm -f $OUTPUT_FILE $ARRAY_FILE $VARIABLE_FILE
mkdir -p $BIN_DIR

# Compilar
echo -e "${YELLOW}[2/4] Compilando arquivos Java...${NC}"
javac -d $BIN_DIR -sourcepath $SRC_DIR \
    CBACT01C.java \
    model/*.java \
    service/*.java

if [ $? -ne 0 ]; then
    echo -e "${RED}✗ Erro na compilação!${NC}"
    exit 1
fi

echo -e "${GREEN}✓ Compilação concluída com sucesso!${NC}"
echo ""

# Verificar se arquivo de entrada existe
if [ ! -f "$INPUT_FILE" ]; then
    echo -e "${RED}✗ Arquivo de entrada não encontrado: $INPUT_FILE${NC}"
    echo -e "${YELLOW}  Criando arquivo de exemplo...${NC}"

    cat > $INPUT_FILE << 'EOF'
00000000001|Y|5000.00|10000.00|2000.00|2024-01-15|2027-01-15|20250115|1500.00|0.00|GRP0000001
00000000002|Y|7500.50|15000.00|3000.00|2024-02-20|2027-02-20|20250220|2000.00|500.00|GRP0000001
00000000003|N|-250.00|5000.00|1000.00|2023-12-10|2026-12-10|20241210|100.00|0.00|GRP0000002
00000000004|Y|12000.75|20000.00|4000.00|2024-03-05|2027-03-05|20250305|3000.00|1200.00|GRP0000003
00000000005|Y|3500.00|8000.00|1500.00|2024-01-01|2027-01-01|20250101|800.00|0.00|GRP0000002
EOF

    echo -e "${GREEN}✓ Arquivo de exemplo criado: $INPUT_FILE${NC}"
    echo ""
fi

# Executar
echo -e "${YELLOW}[3/4] Executando programa...${NC}"
echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo ""

java -cp $BIN_DIR com.aws.carddemo.CBACT01C \
    $INPUT_FILE \
    $OUTPUT_FILE \
    $ARRAY_FILE \
    $VARIABLE_FILE

EXIT_CODE=$?

echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo ""

# Verificar resultado
if [ $EXIT_CODE -eq 0 ]; then
    echo -e "${GREEN}✓ Programa executado com sucesso!${NC}"
    echo ""

    # Mostrar arquivos gerados
    echo -e "${YELLOW}[4/4] Arquivos gerados:${NC}"
    echo ""

    if [ -f "$OUTPUT_FILE" ]; then
        LINES=$(wc -l < $OUTPUT_FILE)
        SIZE=$(ls -lh $OUTPUT_FILE | awk '{print $5}')
        echo -e "  ${GREEN}✓${NC} $OUTPUT_FILE (${LINES} linhas, ${SIZE})"
    fi

    if [ -f "$ARRAY_FILE" ]; then
        LINES=$(wc -l < $ARRAY_FILE)
        SIZE=$(ls -lh $ARRAY_FILE | awk '{print $5}')
        echo -e "  ${GREEN}✓${NC} $ARRAY_FILE (${LINES} linhas, ${SIZE})"
    fi

    if [ -f "$VARIABLE_FILE" ]; then
        LINES=$(wc -l < $VARIABLE_FILE)
        SIZE=$(ls -lh $VARIABLE_FILE | awk '{print $5}')
        echo -e "  ${GREEN}✓${NC} $VARIABLE_FILE (${LINES} linhas, ${SIZE})"
    fi

    echo ""
    echo -e "${BLUE}Para visualizar os arquivos:${NC}"
    echo -e "  cat $OUTPUT_FILE"
    echo -e "  cat $ARRAY_FILE"
    echo -e "  cat $VARIABLE_FILE"

else
    echo -e "${RED}✗ Programa falhou com código de erro: $EXIT_CODE${NC}"
    exit $EXIT_CODE
fi

echo ""
echo -e "${GREEN}Processo concluído!${NC}"
