use apnum::BigInt;

type SmallInt = i32;

#[derive(Clone)]
pub enum Integer {
    Small(SmallInt),
    Big(BigInt)
}

impl std::fmt::Display for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Integer::*;

        match self {
            Small(small) => write!(f, "{small}"),
            Big(big) => write!(f, "{big}"),
        }
    }
}

impl std::cmp::Eq for Integer {}

impl std::cmp::PartialEq for Integer {
    fn eq(&self, other: &Self) -> bool {
        use Integer::*;
        
        match (self, other) {
            (Small(lint), Small(rint)) => lint == rint,
            (Big(lbigint), Big(rbigint)) => lbigint == rbigint,
            (Small(small), Big(big)) => big.eq_i32(*small),
            (Big(big), Small(small)) => big.eq_i32(*small),
        }
    }
}

impl std::cmp::PartialOrd for Integer {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl std::cmp::Ord for Integer {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use Integer::*;

        match (self, other) {
            (Small(lint), Small(rint)) => lint.cmp(rint),
            (Big(lbigint), Big(rbigint)) => lbigint.cmp(rbigint),
            (Small(small), Big(big)) => big.cmp_i32(*small).reverse(),
            (Big(big), Small(small)) => big.cmp_i32(*small),
        }
    }
}

impl std::ops::Add for &Integer {
    type Output = Integer;

    fn add(self, rhs: Self) -> Self::Output {
        use Integer::*;

        match (self, rhs) {
            (Small(lint), Small(rint)) => match lint.checked_add(*rint) {
                Some(int) => Small(int),
                None => Big(BigInt::from(*lint) + *rint),
            },
            (Big(lbigint), Big(rbigint)) => Big(lbigint + rbigint),
            (Big(bigint), Small(int)) => Big(bigint + *int),
            (Small(int), Big(bigint)) => Big(*int + bigint),
        }
    }
}

impl std::ops::Sub for &Integer {
    type Output = Integer;

    fn sub(self, rhs: Self) -> Self::Output {
        use Integer::*;

        match (self, rhs) {
            (Small(lint), Small(rint)) => match lint.checked_sub(*rint) {
                Some(int) => Small(int),
                None => Big(BigInt::from(*lint) - *rint),
            },
            (Big(lbigint), Big(rbigint)) => Big(lbigint - rbigint),
            (Big(bigint), Small(int)) => Big(bigint - *int),
            (Small(int), Big(bigint)) => Big(*int - bigint),
        }
    }
}

impl std::ops::Mul for &Integer {
    type Output = Integer;

    fn mul(self, rhs: Self) -> Self::Output {
        use Integer::*;

        match (self, rhs) {
            (Small(lint), Small(rint)) => match lint.checked_mul(*rint) {
                Some(int) => Small(int),
                None => Big(BigInt::from(*lint) * *rint),
            },
            (Big(lbigint), Big(rbigint)) => Big(lbigint * rbigint),
            (Big(bigint), Small(int)) => Big(bigint * *int),
            (Small(int), Big(bigint)) => Big(*int * bigint),
        }
    }
}

impl std::ops::Div for &Integer {
    type Output = Integer;

    fn div(self, rhs: Self) -> Self::Output {
        use Integer::*;

        match (self, rhs) {
            (Small(lint), Small(rint)) => match lint.checked_div(*rint) {
                Some(int) => Small(int),
                None => Big((BigInt::from(*lint) / *rint).0),
            },
            (Big(lbigint), Big(rbigint)) => Big((lbigint / rbigint).0),
            (Big(bigint), Small(int)) => Big((bigint / *int).0),
            (Small(int), Big(bigint)) => Big((&BigInt::from(*int) / bigint).0),
        }
    }
}

impl std::ops::Neg for &Integer {
    type Output = Integer;

    fn neg(self) -> Self::Output {
        use Integer::*;

        match self {
            Small(int) => Small(-int),
            Big(bigint) => Big(-bigint),
        }
    }
}