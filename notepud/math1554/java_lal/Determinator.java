/*

 @param size of square matrix to generate formula for. 1 is 1*1 matrix

 @purpose Returns a string formula to calculate the determinant of a matrix

*/

public static String generateFormula (int level) {
	
	if (level == 1) {
	
		return "(0,0)";
	
	} else {
		
		String lowerFormula = generateFormula(level - 1);


		for (int i = 0; i < level; i++) {

			

		}

	}

}

/*

 @param coordinate: String coord "(#,#)"
 @param translateBy: corresponds to (x,y) and false = 0, true = +1 

*/

private static long[] getLongs (String coordinate) {

	Scanner getLongs = new Scanner(coordinate);

	return new long[] {getLongs.nextLong(), getLongs.nextLong()};

}

private static String translateCoordinate (String coordinate, boolean[] translateBy) {

	Scanner getLongs = new Scanner(coordinate);

	long first = getLongs.nextLong();
	
	long second = getLongs.nextLong();

	if (translateBy[0]) {

		first++;

	}

	if (translateBy[1]) {

		second++;

	}

	return String.format("(%d,%d)", first, second);

}

private static double applyFormula (String formula, double[][] matrix) {
	
}