public class Matrix {
	private int[][] intMatrix;
	private int rows;
	private int cols;
	
	public Matrix(int rows, int cols) {
		intMatrix = new int[rows][cols];
	}
	public Matrix(int rows, boolean identity) {
		this(rows, rows);
		
		if (identity) {
			for (int r = 0; r < rows; r++) {
				for (int c = 0; c < rows; c++) {
					intMatrix[r][c] = (r == c) ? 1 : 0;
				}
			}			
		}
	}
	public Matrix(int rows) {
		this(rows, rows);
	}
	public Matrix(int rows, int cols, int[] data) {
		intMatrix = new int[rows][cols];
		if (data.length == rows * cols) {
			int marker = 0;
			for (int r = 0; r < rows; r++) {
				for (int c = 0; c < cols; c++) {
					marker++;
					intMatrix[r][marker % cols] = data[marker];	
				}
			}
		} else {
			System.err.println("TMI pls reduc");
			System.exit(0);
		}
	}
	public Matrix(int[][] values) {
		intMatrix = values;
	}
	public Matrix() {
		intMatrix = new int[10][10];
	}
	public int numNonzeroes(int[] arr) {
		int crement = 0;
		for (int i : arr) {
			crement++;
		}
		return crement;
	}

	//caution - this only works predictably if rows are of equal length b/c it sorts by leading index, regardless of total length
	class RowComparator implements java.util.Comparator<int[]> {
		public RowComparator () {}
		public int compare(int[] a, int[] b) {
			int aLeading = getLeading(a)[1];
			int bLeading = getLeading(b)[1];
			
			return aLeading - bLeading;
		}
	}

	public void flipRows(int flipee, int flipend, Matrix m) {
		int[] oldFlipee = m.getRow(flipee).clone();
		m.setRow(flipee, m.getRow(flipend));
		m.setRow(flipend, oldFlipee);
	}

	public int[] addRows(int firstRow, int firstScale, int secondRow, int secondScale) {
		int[] first = intMatrix[firstRow];
		int[] second = intMatrix[secondRow];
	}

	public int[][] getIntMatrix() {
		return intMatrix;
	}

	public int[] getRow(int r) {
		return intMatrix[r];
	}

	public void setRow(int dex, int[] r) {
		intMatrix[dex] = r;
	}

	/*
	takes a row
	returns int[2] where int[0] is leading value, int[1] is index
	*/
	public static int[] getLeading(int[] row) {
		int[] leading = {0, row.length - 1};
		for (int i = 0; i < row.length; i++) {
			int num = row[i];
			if (num == 0) {
				continue;
			} else {
				leading[0] = num;
				leading[1] = i;
				return leading;
			}
		}
		return leading;
	}

	//sorts intmatrix so leftmost leading value is on top
	public void sortRows() {
		java.util.Arrays.sort(intMatrix, new RowComparator());
	}
}