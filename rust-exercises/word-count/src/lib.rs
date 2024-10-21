use std::collections::HashMap;

/// Count occurrences of words.
pub fn word_count(phrase: &str) -> HashMap<String, u32> {
  let mut result = HashMap::new();

  phrase
      .split_whitespace()
      .map(|x| x.to_lowercase())
      .for_each(|x| { result.entry(x).and_modify(|y| *y += 1).or_insert(1); });

  result
}