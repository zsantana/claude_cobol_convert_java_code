package com.aws.carddemo.model;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Representa o registro de conta lido do arquivo VSAM ACCTFILE
 * Equivalente ao ACCOUNT-RECORD do copybook CVACT01Y
 *
 * Mapeamento COBOL -> Java:
 * - PIC 9(11)        -> Long
 * - PIC X(01)        -> String
 * - PIC S9(10)V99    -> BigDecimal
 * - PIC X(10)        -> String ou LocalDate
 */
public class AccountRecord {

    // ACCT-ID - PIC 9(11)
    private Long accountId;

    // ACCT-ACTIVE-STATUS - PIC X(01)
    private String activeStatus;

    // ACCT-CURR-BAL - PIC S9(10)V99
    private BigDecimal currentBalance;

    // ACCT-CREDIT-LIMIT - PIC S9(10)V99
    private BigDecimal creditLimit;

    // ACCT-CASH-CREDIT-LIMIT - PIC S9(10)V99
    private BigDecimal cashCreditLimit;

    // ACCT-OPEN-DATE - PIC X(10)
    private String openDate;

    // ACCT-EXPIRAION-DATE - PIC X(10)
    private String expirationDate;

    // ACCT-REISSUE-DATE - PIC X(10)
    private String reissueDate;

    // ACCT-CURR-CYC-CREDIT - PIC S9(10)V99
    private BigDecimal currentCycleCredit;

    // ACCT-CURR-CYC-DEBIT - PIC S9(10)V99 COMP-3
    private BigDecimal currentCycleDebit;

    // ACCT-GROUP-ID - PIC X(10)
    private String groupId;

    // Constructors
    public AccountRecord() {
    }

    public AccountRecord(Long accountId, String activeStatus, BigDecimal currentBalance,
                        BigDecimal creditLimit, BigDecimal cashCreditLimit, String openDate,
                        String expirationDate, String reissueDate, BigDecimal currentCycleCredit,
                        BigDecimal currentCycleDebit, String groupId) {
        this.accountId = accountId;
        this.activeStatus = activeStatus;
        this.currentBalance = currentBalance;
        this.creditLimit = creditLimit;
        this.cashCreditLimit = cashCreditLimit;
        this.openDate = openDate;
        this.expirationDate = expirationDate;
        this.reissueDate = reissueDate;
        this.currentCycleCredit = currentCycleCredit;
        this.currentCycleDebit = currentCycleDebit;
        this.groupId = groupId;
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
        return "AccountRecord{" +
                "accountId=" + accountId +
                ", activeStatus='" + activeStatus + '\'' +
                ", currentBalance=" + currentBalance +
                ", creditLimit=" + creditLimit +
                ", cashCreditLimit=" + cashCreditLimit +
                ", openDate='" + openDate + '\'' +
                ", expirationDate='" + expirationDate + '\'' +
                ", reissueDate='" + reissueDate + '\'' +
                ", currentCycleCredit=" + currentCycleCredit +
                ", currentCycleDebit=" + currentCycleDebit +
                ", groupId='" + groupId + '\'' +
                '}';
    }
}
