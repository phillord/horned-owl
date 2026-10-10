// Writes the table of `src/java_char.rs`: the runs of UTF-16 code units
// `Character.isLetter` or `Character.isDigit` holds of, under the running JVM.
//
//     javac --release 11 dev/LettersAndDigits.java -d /tmp
//     java -cp /tmp LettersAndDigits > table.txt   (with a Java 11 runtime)
public class LettersAndDigits {
    public static void main(String[] args) {
        StringBuilder out = new StringBuilder();
        int n = 0, lo = -1;
        for (int c = 0; c <= 0x10000; c++) {
            boolean in = c < 0x10000 && (Character.isLetter((char) c) || Character.isDigit((char) c));
            if (in && lo < 0) lo = c;
            if (!in && lo >= 0) {
                out.append(String.format("(0x%04X, 0x%04X),%s", lo, c - 1, (++n % 5 == 0) ? "\n" : " "));
                lo = -1;
            }
        }
        System.out.print(out.toString().trim());
        System.err.println(n + " ranges, Java " + System.getProperty("java.version"));
    }
}
