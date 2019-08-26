import java.util.Scanner;
import java.io.IOException;
import java.io.File;
import java.util.ArrayList;
import java.math.BigDecimal;

public class Matrix {
	private int[][] intMatrix;
	private int rows;
	private int cols;
	private boolean augmented;

	public static void main(String[] args) throws IOException {
		String fileName = args[0];

		if (null == fileName) {
			System.out.println("Input filename to read from: ");
			Scanner s = new Scanner(System.in);
			fileName = s.nextLine();
		}

		Matrix[] epsilon = readMatrixFromFile(fileName);
		epsilon[0].sortRows();
		epsilon[0].reduce();
		System.out.println(epsilon[0]);
	}

	//Constructors
	public Matrix(int rows, int cols) {intMatrix = new int[rows][cols];}
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
					intMatrix[r][marker % cols] = data[marker];
					marker++;	
				}
			}
		} else if (data.length > rows * cols) {
			System.err.println("TMI pls reduc");
			System.exit(0);
		} else {
			System.err.println("NEI pls incres");
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

	public static Matrix[] readMatrixFromFile(String fileName) throws IOException {
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
			matrices.add(new Matrix(nums[0], nums[1], java.util.Arrays.copyOfRange(nums, 2, nums.length)));
		}
		Matrix[] finalMatrices = new Matrix[matrices.size()];
		return matrices.toArray(finalMatrices);
	}

	public static String toString(int[] xs) {
		String stringified = "{ ";
		for (int i = 0; i < xs.length; i++) {
			stringified += String.format("%2d", xs[i]) + (i != xs.length - 1 ? ", " : " ");
		}
		return stringified + "}";
	}

	public static String toString(int[] xs, int padSpace) {
		String stringified = "{ ";
		String formatString = "%" + padSpace + "d";
		for (int i = 0; i < xs.length; i++) {
			stringified += String.format(formatString, xs[i]) + (i != xs.length - 1 ? ", " : " ");
		}
		return stringified + "}";
	}
	
	public int largestNumSize() {
		int largest = 0;
		for (int[] row : intMatrix) {
			for (int i : row) {
				int iLen = Integer.toString(i).length();
				largest = iLen > largest ? iLen : largest;
			}
		}
		return largest;
	}

	public String toString() {
		String stringified = "{ ";
		int largestNumSize = largestNumSize();

		for(int i = 0; i < intMatrix.length; i++) {
			stringified += (i == 0 ? "" : "  ") + toString(intMatrix[i], largestNumSize) + (i != intMatrix.length - 1 ? "\n" : " }");
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

	public void addRows(int firstRow, int firstScale, int secondRow, int secondScale) {
		intMatrix[firstRow] = addRows(intMatrix[firstRow], firstScale, intMatrix[secondRow], secondScale);
	}

	public static int[] scaleRow(int[] row, int scaleBy) {
		int[] scaledRow = row;

		for(int i = 0; i < row.length; i++) {
			scaledRow[i] *= scaleBy;
		}

		return scaledRow;
	}

	public void scaleRow(int row, int scaleBy) {
		intMatrix[row] = scaleRow(intMatrix[row], scaleBy);
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

	//reduces to rref
	public void reduce() {
		sortRows();
		for (int i = 0; i < intMatrix.length; i++) {
			int pivot = getLeading(intMatrix[i])[0];
			int pivotPosition =  getLeading(intMatrix[i])[1];

			if (pivot != 0 && pivot != 1 && i < intMatrix.length && getLeading(intMatrix[i + 1])[1] == pivotPosition) {
				int pivotNext = getLeading(intMatrix[i + 1])[0];

				int pivotMult = 0;

				if (pivot / pivotNext < 0) {
					while (!((pivotNext * pivotMult) % pivot == 1 || (pivotNext * pivotMult) % pivot == -1)) {
						pivotMult++;
						// System.out.println(".");
					}
				} else {
					while (!((pivotNext * pivotMult) % pivot == 1 || (pivotNext * pivotMult) % pivot == -1)) {
						pivotMult--;
						System.out.println((pivotNext * pivotMult) % pivot);
					}	
				}
				intMatrix[i] = addRows(intMatrix[i], (pivotNext * pivotMult) / pivot, intMatrix[i + 1], pivotMult);
			}
		}
	}
}