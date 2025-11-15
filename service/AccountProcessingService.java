package com.aws.carddemo.service;

import com.aws.carddemo.model.*;

import java.io.IOException;

/**
 * Serviço principal de processamento
 * Equivalente à PROCEDURE DIVISION do COBOL (linhas 140-160)
 *
 * Orquestra todo o fluxo de processamento:
 * 1. Abre arquivos
 * 2. Loop de leitura e processamento
 * 3. Fecha arquivos
 */
public class AccountProcessingService {

    private final FileIOService fileIOService;
    private int recordsProcessed = 0;
    private int recordsWithErrors = 0;

    public AccountProcessingService(FileIOService fileIOService) {
        this.fileIOService = fileIOService;
    }

    /**
     * Processa todos os registros do arquivo de entrada
     * Equivalente ao fluxo principal (linhas 141-160)
     *
     * PERFORM UNTIL END-OF-FILE = 'Y'
     *   IF END-OF-FILE = 'N'
     *     PERFORM 1000-ACCTFILE-GET-NEXT
     *     IF END-OF-FILE = 'N'
     *       DISPLAY ACCOUNT-RECORD
     *     END-IF
     *   END-IF
     * END-PERFORM
     *
     * @throws IOException Se houver erro de I/O
     */
    public void processAllAccounts() throws IOException {
        System.out.println("START OF EXECUTION OF PROGRAM CBACT01C");
        System.out.println("===========================================");

        try {
            // Abre todos os arquivos
            fileIOService.openFiles();

            // Loop principal de processamento
            boolean endOfFile = false;
            while (!endOfFile) {
                try {
                    // Lê próximo registro (1000-ACCTFILE-GET-NEXT)
                    AccountRecord account = fileIOService.readNextAccount();

                    if (account == null) {
                        // EOF detectado
                        endOfFile = true;
                        continue;
                    }

                    // Processa o registro
                    processAccountRecord(account);
                    recordsProcessed++;

                } catch (Exception e) {
                    System.err.println("Erro ao processar registro: " + e.getMessage());
                    recordsWithErrors++;

                    // No COBOL original, erros causam ABEND (linhas 192-195)
                    // Aqui podemos decidir se continuamos ou paramos
                    // throw new RuntimeException("Erro fatal no processamento", e);
                }
            }

        } finally {
            // Fecha arquivos (9000-ACCTFILE-CLOSE)
            fileIOService.closeFiles();
        }

        System.out.println("===========================================");
        System.out.println("END OF EXECUTION OF PROGRAM CBACT01C");
        System.out.println("Total de registros processados: " + recordsProcessed);
        System.out.println("Total de registros com erro: " + recordsWithErrors);
    }

    /**
     * Processa um único registro de conta
     * Equivalente às linhas 166-178 do COBOL
     *
     * Executa:
     * 1. Exibe registro (1100-DISPLAY-ACCT-RECORD)
     * 2. Transforma e grava em OUTFILE (1300-POPUL, 1350-WRITE)
     * 3. Transforma e grava em ARRYFILE (1400-POPUL, 1450-WRITE)
     * 4. Transforma e grava em VBRCFILE (1500-POPUL, 1550-WRITE, 1575-WRITE)
     *
     * @param account Registro da conta a processar
     * @throws IOException Se houver erro de I/O
     */
    private void processAccountRecord(AccountRecord account) throws IOException {

        // 1. Exibe o registro (1100-DISPLAY-ACCT-RECORD)
        AccountTransformationService.displayAccountRecord(account);

        // 2. Transforma e grava em OUTFILE (1300-POPUL-ACCT-RECORD + 1350-WRITE)
        OutputAccountRecord outputRecord =
            AccountTransformationService.transformToOutputRecord(account);
        fileIOService.writeOutputRecord(outputRecord);

        // 3. Transforma e grava em ARRYFILE (1400-POPUL-ARRAY-RECORD + 1450-WRITE)
        ArrayAccountRecord arrayRecord =
            AccountTransformationService.transformToArrayRecord(account);
        fileIOService.writeArrayRecord(arrayRecord);

        // 4. Transforma e grava registros variáveis em VBRCFILE
        // (1500-POPUL-VBRC-RECORD + 1550-WRITE-VB1 + 1575-WRITE-VB2)

        // Formata data primeiro para usar em VB2
        String formattedDate = DateFormatter.formatDate(account.getReissueDate());

        // Cria e grava VB1-RECORD
        VariableRecord.VBRecord1 vbRecord1 =
            AccountTransformationService.transformToVBRecord1(account);
        fileIOService.writeVariableRecord1(vbRecord1);
        System.out.println("VBRC-REC1: " + vbRecord1);

        // Cria e grava VB2-RECORD
        VariableRecord.VBRecord2 vbRecord2 =
            AccountTransformationService.transformToVBRecord2(account, formattedDate);
        fileIOService.writeVariableRecord2(vbRecord2);
        System.out.println("VBRC-REC2: " + vbRecord2);
    }

    /**
     * Retorna estatísticas do processamento
     */
    public String getProcessingStatistics() {
        return String.format(
            "Processamento concluído:\n" +
            "  - Registros processados com sucesso: %d\n" +
            "  - Registros com erro: %d\n" +
            "  - Total: %d",
            recordsProcessed, recordsWithErrors, recordsProcessed + recordsWithErrors
        );
    }
}
