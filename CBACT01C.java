package com.aws.carddemo;

import com.aws.carddemo.service.AccountProcessingService;
import com.aws.carddemo.service.FileIOService;

import java.io.File;

/**
 * Classe principal - Conversão do programa COBOL CBACT01C.cbl para Java
 *
 * PROGRAMA ORIGINAL COBOL:
 * ========================
 * PROGRAM-ID    : CBACT01C
 * Application   : CardDemo
 * Type          : BATCH COBOL Program
 * Function      : READ THE ACCOUNT FILE AND WRITE INTO FILES
 * Author        : AWS
 *
 * CONVERSÃO JAVA:
 * ===============
 * Este programa Java replica a funcionalidade do programa COBOL batch CBACT01C.cbl
 *
 * Funcionalidade:
 * - Lê arquivo de contas (ACCTFILE - simulação de VSAM KSDS)
 * - Gera 3 arquivos de saída:
 *   1. OUTFILE  : Registros formatados
 *   2. ARRYFILE : Registros com arrays
 *   3. VBRCFILE : Registros de comprimento variável
 *
 * Uso:
 *   java CBACT01C <inputFile> <outputFile> <arrayFile> <variableFile>
 *
 * Exemplo:
 *   java CBACT01C accounts.dat output.dat array.dat variable.dat
 */
public class CBACT01C {

    private static final String PROGRAM_NAME = "CBACT01C";
    private static final String VERSION = "Java v1.0 (Converted from COBOL)";

    /**
     * Método principal
     *
     * @param args Argumentos da linha de comando:
     *             args[0] - Arquivo de entrada (ACCTFILE)
     *             args[1] - Arquivo de saída (OUTFILE)
     *             args[2] - Arquivo de array (ARRYFILE)
     *             args[3] - Arquivo de registros variáveis (VBRCFILE)
     */
    public static void main(String[] args) {
        System.out.println("╔════════════════════════════════════════════════════════╗");
        System.out.println("║  CBACT01C - Account File Processing Program           ║");
        System.out.println("║  " + VERSION + "                              ║");
        System.out.println("╚════════════════════════════════════════════════════════╝");
        System.out.println();

        // Valida argumentos
        if (args.length != 4) {
            printUsage();
            System.exit(1);
        }

        String inputFile = args[0];
        String outputFile = args[1];
        String arrayFile = args[2];
        String variableFile = args[3];

        // Valida existência do arquivo de entrada
        if (!new File(inputFile).exists()) {
            System.err.println("ERRO: Arquivo de entrada não encontrado: " + inputFile);
            System.exit(8);
        }

        // Cria serviços
        FileIOService fileIOService = new FileIOService(
            inputFile, outputFile, arrayFile, variableFile
        );

        AccountProcessingService processingService =
            new AccountProcessingService(fileIOService);

        // Executa processamento
        int exitCode = 0;
        try {
            processingService.processAllAccounts();
            System.out.println();
            System.out.println(processingService.getProcessingStatistics());

        } catch (Exception e) {
            System.err.println();
            System.err.println("╔════════════════════════════════════════════════════════╗");
            System.err.println("║  ERRO FATAL NO PROCESSAMENTO                           ║");
            System.err.println("╚════════════════════════════════════════════════════════╝");
            System.err.println("Mensagem: " + e.getMessage());
            e.printStackTrace();

            // Equivalente ao ABEND do COBOL (9999-ABEND-PROGRAM, linha 406-410)
            exitCode = 999;
        }

        System.out.println();
        System.out.println("Programa finalizado com código de retorno: " + exitCode);
        System.exit(exitCode);
    }

    /**
     * Exibe instruções de uso
     */
    private static void printUsage() {
        System.err.println("Uso: java CBACT01C <inputFile> <outputFile> <arrayFile> <variableFile>");
        System.err.println();
        System.err.println("Parâmetros:");
        System.err.println("  inputFile    : Arquivo de entrada com dados de contas (ACCTFILE)");
        System.err.println("  outputFile   : Arquivo de saída formatado (OUTFILE)");
        System.err.println("  arrayFile    : Arquivo de saída com arrays (ARRYFILE)");
        System.err.println("  variableFile : Arquivo de saída com registros variáveis (VBRCFILE)");
        System.err.println();
        System.err.println("Exemplo:");
        System.err.println("  java CBACT01C accounts.dat output.dat array.dat variable.dat");
    }
}
