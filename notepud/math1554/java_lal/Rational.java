
public class Rational {
	private long numerator;
	private long denominator;

	public static void main (String[] args) {
		Rational rational0 = new Rational(15L, 20L);
		Rational rational1 = new Rational(9L, 24L);
		System.out.println(rational0);
		System.out.println(greatestCommonFactor(228L, 20L));
//		rational0.simplify();
		System.out.println(rational0)
;gre		rational0.subtract(rational1);
		rational0.subtract(rational1);
		System.out.println(rational0);
	}

	// Constructors
	

	public Rational (long numerator, long denominator) {

		this.numerator = numerator;

		this.denominator = denominator;

		simplify();

	}

	public Rational (long numerator) {

		this.numerator = numerator;

		this.denominator = 1;

	}

	public Rational () {
		this.numerator = 1;
		this.denominator = 1;
	}

	public String toString () {

		return String.format("%d / %d = %1.5e", numerator, denominator, (double) numerator / denominator);

	}

	// Algebraic functions non-static

	public void multiply (Rational multipland) {

		numerator *= multipland.getNumerator();

		denominator *= multipland.getDenominator();

		simplify();

	}

	public void add (Rational addend) {

		numerator *= addend.getDenominator();

		numerator += addend.getNumerator() * getDenominator();

		denominator *= addend.getDenominator();

		// simplifies the result
		simplify();

	}

	public void subtract (Rational subtractend) {
		
		Rational flippedSubtractend = new Rational(subtractend.getNumerator() * -1L, 
			subtractend.getDenominator());

		add(flippedSubtractend);

	}

	public void scale (long coefficient) {
		
		numerator *= coefficient;
		
		denominator *= coefficient;
	
	}

	public void simplify () {
	
		long gcf = greatestCommonFactor (numerator, denominator);
	
		if (gcf != 0) {
	
			numerator /= gcf;
	
			denominator /= gcf;			
	
		}
	
	} 

	// Algebraic static functions

	// Utility methods

	public long getNumerator () {
	
		return numerator;
	
	}

	public long getDenominator () {
	
		return denominator;
	
	}

	private static long greatestCommonFactor (long first, long second) {
	
		long a = Math.abs(first);
	
		long b = Math.abs(second);

		if (first == 0 || second == 0) {
	
			return 0;
	
		} else if (first == second) {
	
			return first;
	
		} else {
	
			long gap;
	
			if (a >= b) {
	
				gap = a - b;
	
				return greatestCommonFactor (gap, b);
	
			} else {
	
				gap = b - a;
	
				return greatestCommonFactor (gap, a);
	
			}
	
		}
	
	}

	private static Rational reduce (Rational number) {
	
		long gcf = greatestCommonFactor (number.numerator, number.denominator);
	
		number.numerator /= gcf;
	
		number.denominator /= gcf;

		return number;

	}

}