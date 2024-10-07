use std::ops;

#[derive(Debug, PartialEq, Eq)]
pub struct Fraction(pub i32, pub i32);

impl Fraction {
    pub fn add(&self, other: Fraction) -> Fraction {
        let scaled_num_self: i32 = self.0 * other.1;
        let scaled_num_other: i32 = other.0 * self.1;
        let scaled_denom: i32 = self.1 * other.1;

        let unsimplified = Fraction(
            scaled_num_self + scaled_num_other,
            scaled_denom
        );
        simplify(unsimplified.0, unsimplified.1)
    }

    pub fn sub(&self, other: Fraction) -> Fraction {
        self.add(Fraction(-other.0, other.1))
    }

    pub fn mul(&self, other: Fraction) -> Fraction {
        let unsimplified = Fraction(&self.0 * other.0, &self.1 * other.1);
        simplify(unsimplified.0, unsimplified.1)
    }

    pub fn divide(&self, other: Fraction) -> Fraction {
        self.mul(Fraction(other.1, other.0))
    }
}

impl ops::Add for Fraction {
    type Output = Fraction;

    fn add(self, other: Fraction) -> Fraction {
        Fraction::add(&self, other)
    }
}

/// Calculate the Highest common factor between 2 numbers
fn hcf(a: i32, b: i32) -> i32 {
    if b == 0 { a } else { hcf(b, a % b) }
}

fn simplify(n: i32, d: i32) -> Fraction {
    let h = hcf(n, d);
    Fraction(n/h, d/h)
}