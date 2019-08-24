import java.util.Scanner;
import java.io.IOException;
import java.io.File;
import java.util.ArrayList;

public class Matrix {
	private int[][] intMatrix;
	private int rows;
	private int cols;
	private boolean augmented;

	public static void main(String[] args) throws IOException {
		String fileName;
		System.out.println("Input filename to read from: ");
		try (Scanner s = new Scanner(System.in)) {
			fileName = s.readLine();
		} catch (IOException e) {
			e.printStackTrace();
		}

		Matrix[] epsilon = readMatrixFromFile(fileName);

	}

	//Constructors
	public Matrix(int rows, int cols) {intMatrix = new int[rows][cols];}
	public Matrix(String fileName) throws IOException {

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
	public Matrix(int rows) {this(rows, rows);}
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
	public Matrix(int[][] values) {intMatrix = values;}
	public Matrix() {intMatrix = new int[10][10];}
	public int numNonzeroes(int[] arr) {
		int crement = 0;
		for (int i : arr) {
			crement++;
		}
		return crement;
	}

	public String toString() {
		
	}

	public Matrix[] readMatrixFromFile(String fileName) throws IOException {
		//holds string form of matrix (rows, cols, data...)
		ArrayList<String> matrixLines = new ArrayList<>();
		ArrayList<Matrix> matrices = new ArrayList<Matrix>();

		try (Scanner matrixReader = new Scanner(new File(fileName))) {	
			while (matrixReader.hasNextLine()) {
				matrixLines.add(matrixReader.nextLine());
			}
		} catch (IOException e) {
			System.err.printf("IOException: %s%n", e);
			System.exit(1);
		}

		for (String matrixLine : matrixLines) {
			String[] numStrings = matrixLine.split(",");
			int[] nums = new int[numStrings.length];
			for(int i = 0; i < numStrings.length; i++) {
				nums[i] = Integer.parseInt(numStrings[i]);
			}
			matrices.add(new Matrix(nums[0], nums[1], java.util.Arrays.copyOfRange(nums, 2, nums.length - 1)));
		}
		Matrix[] finalMatrices = new Matrix[matrices.size()];
		return matrices.toArray(finalMatrices);
	}

	public static String toString(int[] xs) {
		String stringified = "{ ";
		for (int i = 0; i < xs.length; i++) {
			stringified += xs[i] + (i != xs.length - 1 ? ", " : " ");
		}
		return stringified + "}";
	}
	
	public static String toString(int[][] as) {
		String stringified = "{ ";
		
		for(int i = 0; i < as.length; i++) {
			stringified += (i == 0 ? "" : "  ") + toString(as[i]) + (i != as.length - 1 ? "\n" : " }");
		}
		
		return stringified;
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

	//need to update. shouldnt intake matrix
	public void flipRows(int flipee, int flipend, Matrix m) {
		int[] oldFlipee = m.getRow(flipee).clone();
		m.setRow(flipee, m.getRow(flipend));
		m.setRow(flipend, oldFlipee);
	}

	public static int[] addRows(int[] firstRow, int firstScale, int[] secondRow, int secondScale) {
		int[] first = scaleRow(firstRow, firstScale);
		int[] second = scaleRow(secondRow, secondScale);

		int[] last = new int[first.length];

		for (int i = 0; i < first.length; i++) {
			last[i] = first[i] + second[i];
		}

		return last;
	}

	public static int[] scaleRow(int[] row, int scaleBy) {
		int[] scaledRow = row;

		for(int i = 0; i < row.length; i++) {
			scaledRow[i] *= scaleBy;
		}

		return scaledRow;
	}

	public int[][] getintMatrix() {
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
	public int[] getLeading(int[] row) {
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

	//sorts intMatrix so leftmost leading value is on top
	public void sortRows() {
		java.util.Arrays.sort(intMatrix, new RowComparator());
	}
}