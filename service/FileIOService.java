package com.aws.carddemo.service;

import com.aws.carddemo.model.*;

import java.io.*;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

/**
 * Serviço de I/O para leitura e escrita de arquivos
 * Equivalente às operações de OPEN, READ, WRITE, CLOSE do COBOL
 *
 * Arquivos manipulados:
 * - ACCTFILE: Arquivo de entrada (simulação de VSAM KSDS)
 * - OUTFILE: Arquivo de saída 1
 * - ARRYFILE: Arquivo de saída 2 (com arrays)
 * - VBRCFILE: Arquivo de saída 3 (registros variáveis)
 */
public class FileIOService {

    private BufferedReader inputReader;
    private BufferedWriter outputWriter;
    private BufferedWriter arrayWriter;
    private BufferedWriter variableWriter;

    private String inputFilePath;
    private String outputFilePath;
    private String arrayFilePath;
    private String variableFilePath;

    /**
     * Construtor com caminhos dos arquivos
     */
    public FileIOService(String inputFilePath, String outputFilePath,
                        String arrayFilePath, String variableFilePath) {
        this.inputFilePath = inputFilePath;
        this.outputFilePath = outputFilePath;
        this.arrayFilePath = arrayFilePath;
        this.variableFilePath = variableFilePath;
    }

    /**
     * Abre todos os arquivos
     * Equivalente aos paragraphs:
     * - 0000-ACCTFILE-OPEN (linhas 317-333)
     * - 2000-OUTFILE-OPEN (linhas 334-350)
     * - 3000-ARRFILE-OPEN (linhas 352-368)
     * - 4000-VBRFILE-OPEN (linhas 370-386)
     *
     * @throws IOException Se houver erro ao abrir arquivos
     */
    public void openFiles() throws IOException {
        try {
            // Abre arquivo de entrada
            inputReader = new BufferedReader(new FileReader(inputFilePath));
            System.out.println("Arquivo de entrada aberto: " + inputFilePath);

            // Abre arquivos de saída
            outputWriter = new BufferedWriter(new FileWriter(outputFilePath));
            System.out.println("Arquivo de saída aberto: " + outputFilePath);

            arrayWriter = new BufferedWriter(new FileWriter(arrayFilePath));
            System.out.println("Arquivo de array aberto: " + arrayFilePath);

            variableWriter = new BufferedWriter(new FileWriter(variableFilePath));
            System.out.println("Arquivo de registro variável aberto: " + variableFilePath);

        } catch (IOException e) {
            System.err.println("Erro ao abrir arquivos: " + e.getMessage());
            closeFiles(); // Fecha arquivos já abertos
            throw e;
        }
    }

    /**
     * Lê próximo registro do arquivo de entrada
     * Equivalente ao paragraph 1000-ACCTFILE-GET-NEXT (linhas 165-198)
     *
     * @return AccountRecord ou null se fim de arquivo
     * @throws IOException Se houver erro na leitura
     */
    public AccountRecord readNextAccount() throws IOException {
        String line = inputReader.readLine();

        if (line == null) {
            return null; // EOF
        }

        return parseAccountRecord(line);
    }

    /**
     * Parse de linha para AccountRecord
     * Simula o READ ACCTFILE-FILE INTO ACCOUNT-RECORD (linha 166)
     *
     * Formato esperado: campos separados por delimitador (pipe ou CSV)
     */
    private AccountRecord parseAccountRecord(String line) {
        // Formato: ID|STATUS|CURR_BAL|CREDIT_LIM|CASH_LIM|OPEN_DT|EXP_DT|REISS_DT|CYC_CRED|CYC_DEB|GRP_ID
        String[] fields = line.split("\\|");

        AccountRecord record = new AccountRecord();

        try {
            record.setAccountId(Long.parseLong(fields[0].trim()));
            record.setActiveStatus(fields[1].trim());
            record.setCurrentBalance(new BigDecimal(fields[2].trim()));
            record.setCreditLimit(new BigDecimal(fields[3].trim()));
            record.setCashCreditLimit(new BigDecimal(fields[4].trim()));
            record.setOpenDate(fields[5].trim());
            record.setExpirationDate(fields[6].trim());
            record.setReissueDate(fields[7].trim());
            record.setCurrentCycleCredit(new BigDecimal(fields[8].trim()));
            record.setCurrentCycleDebit(new BigDecimal(fields[9].trim()));
            record.setGroupId(fields[10].trim());

        } catch (Exception e) {
            System.err.println("Erro ao parsear registro: " + line);
            throw new RuntimeException("Erro no parse do registro", e);
        }

        return record;
    }

    /**
     * Grava registro no arquivo de saída (OUTFILE)
     * Equivalente ao paragraph 1350-WRITE-ACCT-RECORD (linhas 242-251)
     *
     * @param record Registro a ser gravado
     * @throws IOException Se houver erro na escrita
     */
    public void writeOutputRecord(OutputAccountRecord record) throws IOException {
        outputWriter.write(record.toString());
        outputWriter.newLine();
    }

    /**
     * Grava registro de array (ARRYFILE)
     * Equivalente ao paragraph 1450-WRITE-ARRY-RECORD (linhas 263-274)
     *
     * @param record Registro de array a ser gravado
     * @throws IOException Se houver erro na escrita
     */
    public void writeArrayRecord(ArrayAccountRecord record) throws IOException {
        arrayWriter.write(record.toString());
        arrayWriter.newLine();
    }

    /**
     * Grava registro variável tipo 1 (VBRCFILE)
     * Equivalente ao paragraph 1550-WRITE-VB1-RECORD (linhas 287-300)
     *
     * @param record Registro tipo 1 (12 bytes)
     * @throws IOException Se houver erro na escrita
     */
    public void writeVariableRecord1(VariableRecord.VBRecord1 record) throws IOException {
        // MOVE 12 TO WS-RECD-LEN (linha 288)
        int recordLength = record.getRecordLength();

        // Grava com indicador de tamanho (simula formato VB)
        variableWriter.write(String.format("[%02d]%s", recordLength, record.toString()));
        variableWriter.newLine();
    }

    /**
     * Grava registro variável tipo 2 (VBRCFILE)
     * Equivalente ao paragraph 1575-WRITE-VB2-RECORD (linhas 302-315)
     *
     * @param record Registro tipo 2 (39 bytes)
     * @throws IOException Se houver erro na escrita
     */
    public void writeVariableRecord2(VariableRecord.VBRecord2 record) throws IOException {
        // MOVE 39 TO WS-RECD-LEN (linha 303)
        int recordLength = record.getRecordLength();

        // Grava com indicador de tamanho (simula formato VB)
        variableWriter.write(String.format("[%02d]%s", recordLength, record.toString()));
        variableWriter.newLine();
    }

    /**
     * Fecha todos os arquivos
     * Equivalente ao paragraph 9000-ACCTFILE-CLOSE (linhas 388-404)
     */
    public void closeFiles() {
        try {
            if (inputReader != null) {
                inputReader.close();
                System.out.println("Arquivo de entrada fechado.");
            }
            if (outputWriter != null) {
                outputWriter.flush();
                outputWriter.close();
                System.out.println("Arquivo de saída fechado.");
            }
            if (arrayWriter != null) {
                arrayWriter.flush();
                arrayWriter.close();
                System.out.println("Arquivo de array fechado.");
            }
            if (variableWriter != null) {
                variableWriter.flush();
                variableWriter.close();
                System.out.println("Arquivo de registro variável fechado.");
            }
        } catch (IOException e) {
            System.err.println("Erro ao fechar arquivos: " + e.getMessage());
        }
    }

    /**
     * Verifica se arquivo existe
     */
    public boolean fileExists(String filePath) {
        return Files.exists(Paths.get(filePath));
    }
}
