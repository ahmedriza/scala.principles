import java.io.BufferedReader;
import java.io.FileReader;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Phone {

    public static void main(String[] args) {
        Phone phone = new Phone();
        // System.out.println(phone.wordCode("Java"));
        // System.out.println(phone.wordsForNum.get("5282"));
        Set<List<String>> acc = new HashSet<>();
        phone.encode("47386", acc);
        System.out.println(acc);
    }

    private final List<String> words;
    private final Map<Character, String> mnemonics = new HashMap<>();
    private final Map<Character, Character> charCode = new HashMap<>();
    private final Map<String, List<String>> wordsForNum;

    private Phone() {
        words = getWords();

        mnemonics.put('2', "ABC");
        mnemonics.put('3', "DEF");
        mnemonics.put('4', "GHI");
        mnemonics.put('5', "JKL");
        mnemonics.put('6', "MNO");
        mnemonics.put('7', "PQRS");
        mnemonics.put('8', "TUV");
        mnemonics.put('9', "WXYZ");

        mnemonics.forEach((key, str) -> {
            for (int i = 0; i < str.length(); i++) {
                Character c = str.charAt(i);
                charCode.put(c, key);
            }
        });

        wordsForNum = words.stream().collect(Collectors.groupingBy(this::wordCode));
    }

    public Set<List<String>> encode(String number, Set<List<String>> acc) {
        if (number.isEmpty()) {
            Set<List<String>> result = new HashSet<>();
            result.add(new ArrayList<>());
            return result;
        } else {
            Set<List<String>> result = new HashSet<>();

            for (int i = 1; i <= number.length(); i++) {
                String prefix = number.substring(0, i);
                List<String> ws = wordsForNum.get(prefix);

                if (ws != null) {
                    String suffix = number.substring(i);
                    Set<List<String>> rest = encode(suffix, acc);
                    if (!rest.isEmpty()) {
                        System.out.println("number: " + number + ", prefix: " + prefix
                                + ", ws: " + ws + ": suffix = "
                                + suffix + ", rest: " + rest);
                        rest.add(ws);
                        acc.addAll(rest);
                    }
                }
            }

            return result;
        }
    }

    private String wordCode(String code)  {
        Stream<Character> stream = code.toUpperCase().chars().mapToObj(c -> (char) c);
        List<Character> result = stream.map(charCode::get)
                .collect(Collectors.toList());
        StringBuilder sb = new StringBuilder();
        result.forEach(sb::append);
        return sb.toString();
    }

    List<String> getWords() {
        List<String> words = new ArrayList<>();

        String file = Phone.class.getResource("/linuxwords.txt").getFile();
        try {
            BufferedReader br = new BufferedReader(new FileReader(file));
            String line;
            while ((line = br.readLine()) != null) {
                words.add(line);
            }
        } catch (java.io.IOException e) {
            e.printStackTrace();
        }
        return words;
    }
}
