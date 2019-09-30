public static String generateFormula (int level) {
	if (level == 0) {
		return "(0,0)";
	} else {
		generateFormula(level - 1);
	}
}

/*
 @param coordinate: String coord "(#,#)"
 @param translateBy: corresponds to (x,y) and false = -1, true = 0 
*/

private static String translateCoordinate (String coordinate, boolean[] translateBy) {
	
	Scanner getLongs = new Scanner(coordinate);

	String digits = coordinate.substring(1, coordinate.length() - 1);

}

private Rational applyFormula (String formula, Rational[][] matrix) {
	
}