pub fn sum_of_multiples(limit: u32, factors: &[u32]) -> u32 {
    (1..limit)
        .filter(|x: &u32| factors.iter().any(|y: &u32| *y != 0 && x % y == 0))
        .sum()
}
