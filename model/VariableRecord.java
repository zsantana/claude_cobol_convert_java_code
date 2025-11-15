package com.aws.carddemo.model;

import java.math.BigDecimal;

/**
 * Representa os registros de comprimento variável para o arquivo VBRCFILE
 * Equivalente a VBRC-REC1 e VBRC-REC2 (linhas 123-130 do COBOL)
 *
 * No COBOL, são escritos como registros de tamanho variável (10-80 bytes)
 * VB1: 12 bytes
 * VB2: 39 bytes
 */
public class VariableRecord {

    private RecordType recordType;
    private VBRecord1 record1;
    private VBRecord2 record2;

    public enum RecordType {
        TYPE1,  // 12 bytes
        TYPE2   // 39 bytes
    }

    /**
     * VBRC-REC1 - 12 bytes
     * Linhas 123-125 do COBOL
     */
    public static class VBRecord1 {
        private Long accountId;          // VB1-ACCT-ID - PIC 9(11)
        private String activeStatus;     // VB1-ACCT-ACTIVE-STATUS - PIC X(01)

        public VBRecord1() {
        }

        public VBRecord1(Long accountId, String activeStatus) {
            this.accountId = accountId;
            this.activeStatus = activeStatus;
        }

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

        @Override
        public String toString() {
            return String.format("%011d%s", accountId, activeStatus);
        }

        public int getRecordLength() {
            return 12;
        }
    }

    /**
     * VBRC-REC2 - 39 bytes
     * Linhas 126-130 do COBOL
     */
    public static class VBRecord2 {
        private Long accountId;              // VB2-ACCT-ID - PIC 9(11)
        private BigDecimal currentBalance;   // VB2-ACCT-CURR-BAL - PIC S9(10)V99
        private BigDecimal creditLimit;      // VB2-ACCT-CREDIT-LIMIT - PIC S9(10)V99
        private String reissueYear;          // VB2-ACCT-REISSUE-YYYY - PIC X(04)

        public VBRecord2() {
        }

        public VBRecord2(Long accountId, BigDecimal currentBalance,
                        BigDecimal creditLimit, String reissueYear) {
            this.accountId = accountId;
            this.currentBalance = currentBalance;
            this.creditLimit = creditLimit;
            this.reissueYear = reissueYear;
        }

        public Long getAccountId() {
            return accountId;
        }

        public void setAccountId(Long accountId) {
            this.accountId = accountId;
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

        public String getReissueYear() {
            return reissueYear;
        }

        public void setReissueYear(String reissueYear) {
            this.reissueYear = reissueYear;
        }

        @Override
        public String toString() {
            return String.format("%011d%s%s%s",
                    accountId, currentBalance, creditLimit, reissueYear);
        }

        public int getRecordLength() {
            return 39;
        }
    }

    // Constructors
    public VariableRecord() {
    }

    public static VariableRecord createType1(VBRecord1 record1) {
        VariableRecord vr = new VariableRecord();
        vr.recordType = RecordType.TYPE1;
        vr.record1 = record1;
        return vr;
    }

    public static VariableRecord createType2(VBRecord2 record2) {
        VariableRecord vr = new VariableRecord();
        vr.recordType = RecordType.TYPE2;
        vr.record2 = record2;
        return vr;
    }

    // Getters
    public RecordType getRecordType() {
        return recordType;
    }

    public VBRecord1 getRecord1() {
        return record1;
    }

    public VBRecord2 getRecord2() {
        return record2;
    }

    @Override
    public String toString() {
        if (recordType == RecordType.TYPE1) {
            return "VBR-TYPE1: " + record1.toString();
        } else {
            return "VBR-TYPE2: " + record2.toString();
        }
    }
}
