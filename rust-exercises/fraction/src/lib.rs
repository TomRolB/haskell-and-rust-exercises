type Fraction = (i32, i32);

/// Add 2 fractions
pub fn add((n1, d1): Fraction, (n2, d2): Fraction) -> Fraction {
    let (scaled_n1, scaled_d1): Fraction = scalar_mul((n1, d1), d2);
    let (scaled_n2, _        ): Fraction = scalar_mul((n2, d2), d1);
    simplify(scaled_n1 + scaled_n2, scaled_d1)
}

fn scalar_mul((n, d): Fraction, scalar: i32) -> Fraction {
    (n * scalar, d * scalar)
}

/// Subtract 2 fractions
pub fn sub((n1, d1): Fraction, (n2, d2): Fraction) -> Fraction {
    add((n1, d1), (-n2, d2))
}

/// Multiply 2 fractions
pub fn mul((n1, d1): Fraction, (n2, d2): Fraction) -> Fraction {
    let (n, d): Fraction = (n1 * n2, d1 * d2);
    simplify(n, d)
}

/// Divide 2 fractions
pub fn divide((n1, d1): Fraction, (n2, d2): Fraction) -> Fraction {
    mul((n1, d1), (d2, n2))
}

/// Calculate the Highest common factor between 2 numbers
pub fn hcf(a: i32, b: i32) -> i32 {
    if a == b       { a }
    else if a > b   { hcf(a - b, b) }
    else            { hcf(b - a, a) }
}

/// Create a fraction simplifying with the arguments simplified by the `hcf`
pub fn simplify(n: i32, d: i32) -> Fraction {
    let hcf = hcf(n.abs(), d.abs());
    (n / hcf, d / hcf)
}
