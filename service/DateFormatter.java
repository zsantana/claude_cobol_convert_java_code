package com.aws.carddemo.service;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

/**
 * Serviço de formatação de datas
 * Equivalente à chamada do programa Assembler COBDATFT (linha 231 do COBOL)
 *
 * No COBOL:
 * - CALL 'COBDATFT' USING CODATECN-REC
 * - Entrada: CODATECN-INP-DATE (formato tipo 2)
 * - Saída: CODATECN-0UT-DATE (formato tipo 2)
 *
 * Formato tipo 2: YYYY-MM-DD
 */
public class DateFormatter {

    private static final DateTimeFormatter INPUT_FORMAT = DateTimeFormatter.ofPattern("yyyyMMdd");
    private static final DateTimeFormatter OUTPUT_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd");

    /**
     * Formata data do formato YYYYMMDD para YYYY-MM-DD
     * Simula a chamada COBDATFT do COBOL (linhas 223-233)
     *
     * @param inputDate Data no formato YYYYMMDD (ex: "20250115")
     * @return Data no formato YYYY-MM-DD (ex: "2025-01-15")
     */
    public static String formatDate(String inputDate) {
        if (inputDate == null || inputDate.trim().isEmpty()) {
            return "";
        }

        try {
            // Remove espaços e hífens se existirem
            String cleanDate = inputDate.replaceAll("[\\s-]", "");

            // Se já estiver no formato correto com 8 dígitos
            if (cleanDate.length() == 8) {
                LocalDate date = LocalDate.parse(cleanDate, INPUT_FORMAT);
                return date.format(OUTPUT_FORMAT);
            }

            // Se já tiver hífens, retorna como está
            if (inputDate.contains("-")) {
                return inputDate;
            }

            return inputDate;
        } catch (Exception e) {
            System.err.println("Erro ao formatar data: " + inputDate + " - " + e.getMessage());
            return inputDate;
        }
    }

    /**
     * Extrai apenas o ano da data
     * Usado para popular VB2-ACCT-REISSUE-YYYY (linha 282 do COBOL)
     *
     * @param formattedDate Data no formato YYYY-MM-DD
     * @return Ano (YYYY)
     */
    public static String extractYear(String formattedDate) {
        if (formattedDate == null || formattedDate.length() < 4) {
            return "";
        }
        return formattedDate.substring(0, 4);
    }

    /**
     * Converte data do formato WS-ACCT-REISSUE-DATE para YYYY-MM-DD
     * Equivalente às linhas 131-137 do COBOL
     *
     * @param year Ano (4 dígitos)
     * @param month Mês (2 dígitos)
     * @param day Dia (2 dígitos)
     * @return Data formatada YYYY-MM-DD
     */
    public static String formatDate(String year, String month, String day) {
        return String.format("%s-%s-%s", year, month, day);
    }
}
