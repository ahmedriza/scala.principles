import java.io.BufferedReader;
import java.io.FileReader;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Phone {

    public static void main(String[] args) {
        Phone phone = new Phone();
        List<List<String>> result = phone.encode("7225247386");
        // Set<List<String>> result = phone.encode("47386");
        result.forEach(System.out::println);
    }

    private final Map<Character, Character> charCode = new HashMap<>();
    private final Map<String, List<String>> wordsForNum;

    private Phone() {
        List<String> words = getWords();

        Map<Character, String> mnemonics = new HashMap<>();
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

    private List<List<String>> encode(String number) {
        if (number.isEmpty()) {
            List<List<String>> result = new ArrayList<>();
            result.add(new ArrayList<>());
            return result;
        } else {
            List<List<String>> result = new ArrayList<>();

            for (int i = 1; i <= number.length(); i++) {
                String prefix = number.substring(0, i);
                String suffix = number.substring(i);

                List<String> words = wordsForNum.getOrDefault(prefix, Collections.emptyList());

                for (String  word : words) {

                    List<List<String>> rest = encode(suffix);

                    for (List<String> strings : rest) {
                        result.add(addToList(word, strings));
                    }
                }
            }

            return result;
        }
    }

    private List<String> addToList(String word, List<String> list) {
        List<String> result = new ArrayList<>();
        result.add(word);
        result.addAll(list);
        return result;
    }

    private String wordCode(String code)  {
        Stream<Character> stream = code.toUpperCase().chars().mapToObj(c -> (char) c);
        List<Character> result = stream.map(charCode::get)
                .collect(Collectors.toList());
        StringBuilder sb = new StringBuilder();
        result.forEach(sb::append);
        return sb.toString();
    }

    private List<String> getWords() {
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
