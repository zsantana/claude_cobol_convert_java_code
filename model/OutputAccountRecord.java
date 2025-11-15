package com.aws.carddemo.model;

import java.math.BigDecimal;

/**
 * Representa o registro de saída para o arquivo OUTFILE
 * Equivalente ao OUT-ACCT-REC (linhas 57-69 do COBOL)
 */
public class OutputAccountRecord {

    private Long accountId;                    // OUT-ACCT-ID - PIC 9(11)
    private String activeStatus;               // OUT-ACCT-ACTIVE-STATUS - PIC X(01)
    private BigDecimal currentBalance;         // OUT-ACCT-CURR-BAL - PIC S9(10)V99
    private BigDecimal creditLimit;            // OUT-ACCT-CREDIT-LIMIT - PIC S9(10)V99
    private BigDecimal cashCreditLimit;        // OUT-ACCT-CASH-CREDIT-LIMIT - PIC S9(10)V99
    private String openDate;                   // OUT-ACCT-OPEN-DATE - PIC X(10)
    private String expirationDate;             // OUT-ACCT-EXPIRAION-DATE - PIC X(10)
    private String reissueDate;                // OUT-ACCT-REISSUE-DATE - PIC X(10)
    private BigDecimal currentCycleCredit;     // OUT-ACCT-CURR-CYC-CREDIT - PIC S9(10)V99
    private BigDecimal currentCycleDebit;      // OUT-ACCT-CURR-CYC-DEBIT - PIC S9(10)V99 COMP-3
    private String groupId;                    // OUT-ACCT-GROUP-ID - PIC X(10)

    // Constructors
    public OutputAccountRecord() {
    }

    // Getters and Setters
    public Long getAccountId() {
        return accountId;
    }

    public void setAccountId(Long accountId) {
        this.accountId = accountId;
    }

    public String getActiveStatus() {
        return activeStatus;
    }

    public void setActiveStatus(String activeStatus) {
        this.activeStatus = activeStatus;
    }

    public BigDecimal getCurrentBalance() {
        return currentBalance;
    }

    public void setCurrentBalance(BigDecimal currentBalance) {
        this.currentBalance = currentBalance;
    }

    public BigDecimal getCreditLimit() {
        return creditLimit;
    }

    public void setCreditLimit(BigDecimal creditLimit) {
        this.creditLimit = creditLimit;
    }

    public BigDecimal getCashCreditLimit() {
        return cashCreditLimit;
    }

    public void setCashCreditLimit(BigDecimal cashCreditLimit) {
        this.cashCreditLimit = cashCreditLimit;
    }

    public String getOpenDate() {
        return openDate;
    }

    public void setOpenDate(String openDate) {
        this.openDate = openDate;
    }

    public String getExpirationDate() {
        return expirationDate;
    }

    public void setExpirationDate(String expirationDate) {
        this.expirationDate = expirationDate;
    }

    public String getReissueDate() {
        return reissueDate;
    }

    public void setReissueDate(String reissueDate) {
        this.reissueDate = reissueDate;
    }

    public BigDecimal getCurrentCycleCredit() {
        return currentCycleCredit;
    }

    public void setCurrentCycleCredit(BigDecimal currentCycleCredit) {
        this.currentCycleCredit = currentCycleCredit;
    }

    public BigDecimal getCurrentCycleDebit() {
        return currentCycleDebit;
    }

    public void setCurrentCycleDebit(BigDecimal currentCycleDebit) {
        this.currentCycleDebit = currentCycleDebit;
    }

    public String getGroupId() {
        return groupId;
    }

    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }

    @Override
    public String toString() {
        return String.format("%011d|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s",
                accountId, activeStatus, currentBalance, creditLimit, cashCreditLimit,
                openDate, expirationDate, reissueDate, currentCycleCredit,
                currentCycleDebit, groupId);
    }
}
