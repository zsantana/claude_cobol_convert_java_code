package com.aws.carddemo.model;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * Representa o registro de array para o arquivo ARRYFILE
 * Equivalente ao ARR-ARRAY-REC (linhas 72-78 do COBOL)
 * Demonstra uso de OCCURS (array/tabela)
 */
public class ArrayAccountRecord {

    private Long accountId;  // ARR-ACCT-ID - PIC 9(11)

    // ARR-ACCT-BAL OCCURS 5 TIMES
    private List<BalanceEntry> balanceEntries;

    private String filler;   // ARR-FILLER - PIC X(04)

    /**
     * Classe interna para representar cada ocorrência do array
     * Equivalente a ARR-ACCT-BAL no COBOL
     */
    public static class BalanceEntry {
        private BigDecimal currentBalance;      // ARR-ACCT-CURR-BAL - PIC S9(10)V99
        private BigDecimal currentCycleDebit;   // ARR-ACCT-CURR-CYC-DEBIT - PIC S9(10)V99 COMP-3

        public BalanceEntry() {
        }

        public BalanceEntry(BigDecimal currentBalance, BigDecimal currentCycleDebit) {
            this.currentBalance = currentBalance;
            this.currentCycleDebit = currentCycleDebit;
        }

        public BigDecimal getCurrentBalance() {
            return currentBalance;
        }

        public void setCurrentBalance(BigDecimal currentBalance) {
            this.currentBalance = currentBalance;
        }

        public BigDecimal getCurrentCycleDebit() {
            return currentCycleDebit;
        }

        public void setCurrentCycleDebit(BigDecimal currentCycleDebit) {
            this.currentCycleDebit = currentCycleDebit;
        }

        @Override
        public String toString() {
            return String.format("[Bal=%s, Debit=%s]", currentBalance, currentCycleDebit);
        }
    }

    // Constructors
    public ArrayAccountRecord() {
        this.balanceEntries = new ArrayList<>(5);
    }

    public ArrayAccountRecord(Long accountId) {
        this.accountId = accountId;
        this.balanceEntries = new ArrayList<>(5);
    }

    // Getters and Setters
    public Long getAccountId() {
        return accountId;
    }

    public void setAccountId(Long accountId) {
        this.accountId = accountId;
    }

    public List<BalanceEntry> getBalanceEntries() {
        return balanceEntries;
    }

    public void setBalanceEntries(List<BalanceEntry> balanceEntries) {
        this.balanceEntries = balanceEntries;
    }

    public void addBalanceEntry(BigDecimal currentBalance, BigDecimal currentCycleDebit) {
        this.balanceEntries.add(new BalanceEntry(currentBalance, currentCycleDebit));
    }

    public String getFiller() {
        return filler;
    }

    public void setFiller(String filler) {
        this.filler = filler;
    }

    @Override
    public String toString() {
        return "ArrayAccountRecord{" +
                "accountId=" + accountId +
                ", balanceEntries=" + balanceEntries +
                ", filler='" + filler + '\'' +
                '}';
    }
}
