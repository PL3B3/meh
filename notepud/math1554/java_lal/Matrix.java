public class Matrix<T extends Number> {
	private T[][] typeMatrix;
	private int rows;
	private int cols;
	private boolean augmented;

	public Matrix(int rows, int cols) {
		typeMatrix = new T[rows][cols];
	}
	public Matrix(int rows, boolean identity) {
		this(rows, rows);
		
		if (identity) {
			for (int r = 0; r < rows; r++) {
				for (int c = 0; c < rows; c++) {
					typeMatrix[r][c] = (r == c) ? 1 : 0;
				}
			}			
		}
	}
	public Matrix(int rows) {
		this(rows, rows);
	}
	public Matrix(int rows, int cols, T[] data) {
		typeMatrix = new T[rows][cols];
		if (data.length == rows * cols) {
			int marker = 0;
			for (int r = 0; r < rows; r++) {
				for (int c = 0; c < cols; c++) {
					marker++;
					typeMatrix[r][marker % cols] = data[marker];	
				}
			}
		} else {
			System.err.println("TMI pls reduc");
			System.exit(0);
		}
	}
	public Matrix(T[][] values) {
		typeMatrix = values;
	}
	public Matrix() {
		typeMatrix = new T[10][10];
	}
	public int numNonzeroes(T[] arr) {
		int crement = 0;
		for (T i : arr) {
			crement++;
		}
		return crement;
	}

	//caution - this only works predictably if rows are of equal length b/c it sorts by leading index, regardless of total length
	class RowComparator implements java.util.Comparator<T[]> {
		public RowComparator () {}
		public int compare(T[] a, T[] b) {
			int aLeading = getLeading(a)[1];
			int bLeading = getLeading(b)[1];
			
			return aLeading - bLeading;
		}
	}

	//need to update. shouldnt intake matrix
	public void flipRows(int flipee, int flipend, Matrix m) {
		int[] oldFlipee = m.getRow(flipee).clone();
		m.setRow(flipee, m.getRow(flipend));
		m.setRow(flipend, oldFlipee);
	}

	public T[] addRows(int firstRow, int firstScale, int secondRow, int secondScale) {
		T[] first = typeMatrix[firstRow];
		T[] second = typeMatrix[secondRow];
	}

	public T[][] gettypeMatrix() {
		return typeMatrix;
	}

	public T[] getRow(int r) {
		return typeMatrix[r];
	}

	public void setRow(int dex, T[] r) {
		typeMatrix[dex] = r;
	}

	/*
	takes a row
	returns int[2] where int[0] is leading value, int[1] is index
	*/
	public static T[] getLeading(T[] row) {
		T[] leading = {0, row.length - 1};
		for (int i = 0; i < row.length; i++) {
			T num = row[i];
			if (num == 0) {
				continue;
			} else {
				leading[0] = num;
				leading[1] = (T) i;
				return leading;
			}
		}
		return leading;
	}

	//sorts typeMatrix so leftmost leading value is on top
	public void sortRows() {
		java.util.Arrays.sort(typeMatrix, new RowComparator());
	}
}