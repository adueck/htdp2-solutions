function gcd1(a: number, b: number): number {
    function commonIn(a: number[], b: number[]): number[] {
        return a.filter(x => b.includes(x));
    }
    function findDivisors(n: number): number[] {
        const divisors: number[] = [];
        for (let i = n; i > 0; i--) {
            if (n % i === 0) {
                divisors.push(i);
            }
        }
        return divisors;
    }
    
    const aDivisors = findDivisors(a);
    const bDivisors = findDivisors(b);
    const allCommonDivisors = commonIn(aDivisors, bDivisors);
    return Math.max(...allCommonDivisors);
}

function gcd2(a: number, b: number): number {
    function greatestDivisor(i: number): number {
        if (i === 1) return 1;
        for (let j = i; j > 0; j--) {
            if (j === 1) return j;
            if (((a % j) === 0) && ((b % j) === 0)) {
                return j;
            }
        }
        throw new Error("unable to find divisor");
    }
    return greatestDivisor(Math.min(a, b))
}

function gcdE(a: number, b: number): number {
    if (a === 0) {
        return b;
    }
    if (b === 0) {
        return a;
    }
    // a and and b both !== 0
    const [smaller, larger] = a > b ? [b, a] : [a, b];
    const r = (larger % smaller);
    return gcdE(smaller, r);
}

function gcdF(a: number, b: number): number {
    function cleverGCD(larger: number, smaller: number): number {
        if (smaller === 0) return larger;
        return cleverGCD(smaller, larger % smaller);
    }
    return cleverGCD(Math.max(a, b), Math.min(a, b));
}

const tests: { a: number, b: number, gcd: number }[] = [
    { a: 5, b: 10, gcd: 5 },
    { a: 10, b: 5, gcd: 5 },
    { a: 6, b: 25, gcd: 1 },
    { a: 18, b: 24, gcd: 6 },
];

const functions = [gcd1, gcd2, gcdE, gcdF];

// all tests pass when for every test
const allPassed = tests.every(t => (
    // every function returns the expected gcd
    functions.every(f => f(t.a, t.b) === t.gcd)
));

console.log({ allPassed });