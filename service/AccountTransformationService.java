package com.aws.carddemo.service;

import com.aws.carddemo.model.*;

import java.math.BigDecimal;

/**
 * Serviço responsável pelas transformações e lógica de negócio
 * Equivalente aos paragraphs COBOL:
 * - 1300-POPUL-ACCT-RECORD (linhas 215-240)
 * - 1400-POPUL-ARRAY-RECORD (linhas 253-261)
 * - 1500-POPUL-VBRC-RECORD (linhas 276-285)
 */
public class AccountTransformationService {

    /**
     * Transforma AccountRecord em OutputAccountRecord
     * Equivalente ao paragraph 1300-POPUL-ACCT-RECORD
     *
     * Lógica de negócio:
     * 1. Copia todos os campos
     * 2. Formata data de reissue usando DateFormatter (simula COBDATFT)
     * 3. Se currentCycleDebit = 0, atribui valor padrão 2525.00 (linha 236-238)
     *
     * @param source Registro de entrada
     * @return Registro de saída transformado
     */
    public static OutputAccountRecord transformToOutputRecord(AccountRecord source) {
        OutputAccountRecord output = new OutputAccountRecord();

        // MOVE ACCT-ID TO OUT-ACCT-ID
        output.setAccountId(source.getAccountId());

        // MOVE ACCT-ACTIVE-STATUS TO OUT-ACCT-ACTIVE-STATUS
        output.setActiveStatus(source.getActiveStatus());

        // MOVE ACCT-CURR-BAL TO OUT-ACCT-CURR-BAL
        output.setCurrentBalance(source.getCurrentBalance());

        // MOVE ACCT-CREDIT-LIMIT TO OUT-ACCT-CREDIT-LIMIT
        output.setCreditLimit(source.getCreditLimit());

        // MOVE ACCT-CASH-CREDIT-LIMIT TO OUT-ACCT-CASH-CREDIT-LIMIT
        output.setCashCreditLimit(source.getCashCreditLimit());

        // MOVE ACCT-OPEN-DATE TO OUT-ACCT-OPEN-DATE
        output.setOpenDate(source.getOpenDate());

        // MOVE ACCT-EXPIRAION-DATE TO OUT-ACCT-EXPIRAION-DATE
        output.setExpirationDate(source.getExpirationDate());

        // Formatação de data (simula CALL 'COBDATFT')
        // Linhas 223-233 do COBOL
        String formattedReissueDate = DateFormatter.formatDate(source.getReissueDate());
        output.setReissueDate(formattedReissueDate);

        // MOVE ACCT-CURR-CYC-CREDIT TO OUT-ACCT-CURR-CYC-CREDIT
        output.setCurrentCycleCredit(source.getCurrentCycleCredit());

        // Lógica de negócio: Se debit = 0, atribui 2525.00 (linhas 236-238)
        if (source.getCurrentCycleDebit() == null ||
            source.getCurrentCycleDebit().compareTo(BigDecimal.ZERO) == 0) {
            output.setCurrentCycleDebit(new BigDecimal("2525.00"));
        } else {
            output.setCurrentCycleDebit(source.getCurrentCycleDebit());
        }

        // MOVE ACCT-GROUP-ID TO OUT-ACCT-GROUP-ID
        output.setGroupId(source.getGroupId());

        return output;
    }

    /**
     * Transforma AccountRecord em ArrayAccountRecord
     * Equivalente ao paragraph 1400-POPUL-ARRAY-RECORD (linhas 253-261)
     *
     * Lógica de negócio:
     * - Cria array com 5 ocorrências
     * - Popula com valores hardcoded para teste/demonstração
     * - Inclui valores negativos para testar formatos numéricos
     *
     * @param source Registro de entrada
     * @return Registro de array com 5 ocorrências
     */
    public static ArrayAccountRecord transformToArrayRecord(AccountRecord source) {
        ArrayAccountRecord arrayRecord = new ArrayAccountRecord(source.getAccountId());

        // Ocorrência 1 (linhas 255-256)
        arrayRecord.addBalanceEntry(
            source.getCurrentBalance(),
            new BigDecimal("1005.00")
        );

        // Ocorrência 2 (linhas 257-258)
        arrayRecord.addBalanceEntry(
            source.getCurrentBalance(),
            new BigDecimal("1525.00")
        );

        // Ocorrência 3 com valores negativos (linhas 259-260)
        arrayRecord.addBalanceEntry(
            new BigDecimal("-1025.00"),
            new BigDecimal("-2500.00")
        );

        // Ocorrências 4 e 5 não são populadas no COBOL original
        // mas o array tem 5 posições. Vamos deixar null ou adicionar zeros
        arrayRecord.addBalanceEntry(BigDecimal.ZERO, BigDecimal.ZERO);
        arrayRecord.addBalanceEntry(BigDecimal.ZERO, BigDecimal.ZERO);

        arrayRecord.setFiller("    "); // 4 espaços

        return arrayRecord;
    }

    /**
     * Transforma AccountRecord em VariableRecord Type 1
     * Equivalente ao paragraph 1500-POPUL-VBRC-RECORD (linhas 277-279)
     *
     * @param source Registro de entrada
     * @return Registro variável tipo 1 (12 bytes)
     */
    public static VariableRecord.VBRecord1 transformToVBRecord1(AccountRecord source) {
        return new VariableRecord.VBRecord1(
            source.getAccountId(),
            source.getActiveStatus()
        );
    }

    /**
     * Transforma AccountRecord em VariableRecord Type 2
     * Equivalente ao paragraph 1500-POPUL-VBRC-RECORD (linhas 280-282)
     *
     * @param source Registro de entrada
     * @param formattedReissueDate Data já formatada (YYYY-MM-DD)
     * @return Registro variável tipo 2 (39 bytes)
     */
    public static VariableRecord.VBRecord2 transformToVBRecord2(
            AccountRecord source, String formattedReissueDate) {

        // Extrai apenas o ano (WS-ACCT-REISSUE-YYYY)
        String reissueYear = DateFormatter.extractYear(formattedReissueDate);

        return new VariableRecord.VBRecord2(
            source.getAccountId(),
            source.getCurrentBalance(),
            source.getCreditLimit(),
            reissueYear
        );
    }

    /**
     * Exibe o registro da conta (equivalente a 1100-DISPLAY-ACCT-RECORD)
     * Linhas 200-213 do COBOL
     *
     * @param account Registro a ser exibido
     */
    public static void displayAccountRecord(AccountRecord account) {
        System.out.println("ACCT-ID                 : " + account.getAccountId());
        System.out.println("ACCT-ACTIVE-STATUS      : " + account.getActiveStatus());
        System.out.println("ACCT-CURR-BAL           : " + account.getCurrentBalance());
        System.out.println("ACCT-CREDIT-LIMIT       : " + account.getCreditLimit());
        System.out.println("ACCT-CASH-CREDIT-LIMIT  : " + account.getCashCreditLimit());
        System.out.println("ACCT-OPEN-DATE          : " + account.getOpenDate());
        System.out.println("ACCT-EXPIRAION-DATE     : " + account.getExpirationDate());
        System.out.println("ACCT-REISSUE-DATE       : " + account.getReissueDate());
        System.out.println("ACCT-CURR-CYC-CREDIT    : " + account.getCurrentCycleCredit());
        System.out.println("ACCT-CURR-CYC-DEBIT     : " + account.getCurrentCycleDebit());
        System.out.println("ACCT-GROUP-ID           : " + account.getGroupId());
        System.out.println("-------------------------------------------------");
    }
}
