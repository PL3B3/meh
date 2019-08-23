public class Matrix {
	private int[][] intMatrix;
	private int rows;
	private int cols;
	
	public Matrix(int rows, int cols) {
		intMatrix = new int[rows][cols];
	}
	public Matrix(int rows, boolean identity) {
		this(rows, rows);
		
		if identity {
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
		if (data.length == rows * cols) {
			this(rows, cols);
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

	public void flipRows(int flipee, int flipend, Matrix m) {
		int[] oldFlipee = m.getRow(flipee).clone();
		m.setRow(flipee, m.getRow(flipend));
		m.setRow(flipend, oldFlipee);
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
}