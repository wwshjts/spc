package org.syspro.spc.lexer.utils;


import java.util.Optional;

// Just wrapper around Character unicode constants
public class UnicodeUtils {


    public static char codePointToChar(int cp) {
        if (Character.isSupplementaryCodePoint(cp)) {
            throw new IllegalArgumentException("Can't convert code point: " + cp + "to char cause it is represents surrogate pair");
        } else {
            return Character.toChars(cp)[0]; // TODO: very ugly
        }
    }

    public static Optional<Character> safeCodePointToChar(int cp) {
        if (Character.isSupplementaryCodePoint(cp)) {
            return Optional.empty();
        } else {
            return Optional.of(Character.toChars(cp)[0]); // TODO: very ugly
        }
    }

    public static boolean isNewLine(String ch) {
        assert ch != null;
        return ch.equals("\n") || ch.equals("\r");
    }

    public static boolean isSpace(String ch) {
        assert ch != null;
        return ch.equals(" ") || ch.equals("\t");
    }

    public static int getNumberOfSpaces(String ch) {
        return switch (ch) {
            case " " -> 1;
            case "\t" -> 2; // there is no difference \t equals two or four spaces
            default -> throw new IllegalArgumentException("Can't get number of spaces in non space character: " + ch + ch.codePointAt(0));
        };
    }

    public static boolean isDigit(String s) {
        assert s.length() == 1;
        return switch (s) {
            case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" -> true;
            default -> false;
        };
    }


    public static boolean isSurrogate(int cp) {
        return Character.isSupplementaryCodePoint(cp);
    }

    public static boolean isHexDigit(String s) {
        assert s.length() == 1;
        return switch (s) {
            case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "F" -> true;
            default -> false;
        };
    }
}