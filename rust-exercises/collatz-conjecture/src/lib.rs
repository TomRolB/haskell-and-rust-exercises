pub fn collatz(mut n: u64) -> Option<u64> {
  collatz_recur(n, 0)
}

fn collatz_recur(mut n: u64, step: u64) -> Option<u64> {
  if n < 1           { None }
  else if n == 1     { Some(step) }
  else if n % 2 == 0 { collatz_recur(n / 2, step + 1) }
  else               { collatz_recur(3 * n + 1, step + 1) }
}