import java.util.Scanner;
import java.io.IOException;
import java.io.File;
import java.util.ArrayList;

public class BigMatrix {
	private double[][] bigMatrix;

	public static void main(String[] args) throws IOException {
		if (args.length < 2) {
			System.err.println("provide (String) filename where csv matrix is located, and (int) matrix index for which matrix to use, separated by spaces");
			System.exit(0);
		}

		String fileName = args[0];
		int index = Integer.parseInt(args[1]);
		// if (null == fileName) {
		// 	System.out.println("Input filename to read from: ");
		// 	Scanner s = new Scanner(System.in);
		// 	fileName = s.nextLine();
		// }
		BigMatrix[] jibi = readBigMatrixFromFile(fileName);

		jibi[index].resolve();

		// System.out.println(toString(addRows(a, 1, b, 1)));		
	}

	public void resolve() {
		sortRows();
		reduce();
		echelate();

		System.out.println(this);
		System.out.println();

		for (int i = 0; i < bigMatrix.length; i++) {
			
			int leadingPosition = getLeadingPosition(bigMatrix[i]);
			double leadingNumber = getLeadingNum(bigMatrix[i]);
			String lineEquation = String.format("%4.2f(x%d)", leadingNumber, leadingPosition + 1);

			if (leadingPosition == (bigMatrix[i].length - 1)) {
				if (leadingNumber != 0) {
					System.out.println("This system is inconsistent");
					break;	
				} else {
					break;
				}
			} 

			double[] subArray = java.util.Arrays.copyOfRange(bigMatrix[i], leadingPosition + 1, bigMatrix[i].length);
			for (int j = 0; j < subArray.length - 1; j++) {
				if (subArray[j] != 0) {
					lineEquation += String.format(" + %4.2f(x%d)", subArray[j], leadingPosition + j + 2);
				}
			}

			lineEquation += String.format(" = %4.2f%n", bigMatrix[i][bigMatrix[i].length - 1]);
			System.out.println(lineEquation);
		}
	}

	public void echelate() {
		for (int i = 1; i < bigMatrix.length; i++) {
			for (int d = 0; d < i; d ++) {
				double columnNum = bigMatrix[d][getLeadingPosition(bigMatrix[i])];
				
				if (! (columnNum < 0.00001 && columnNum > -0.00001)) {
					// System.out.println(d);
					bigMatrix[d] = addRows2(bigMatrix[d], 1, bigMatrix[i], -(columnNum));
					// System.out.println(toString(addRows2(getRow(d), 1, getRow(i), -(columnNum))));
				}	
			}
		}
	}

	//Constructors
	public BigMatrix(int rows, int cols) {bigMatrix = new double[rows][cols];}
	public BigMatrix(int rows, boolean identity) {
		this(rows, rows);
		
		if (identity) {
			for (int r = 0; r < rows; r++) {
				for (int c = 0; c < rows; c++) {
					bigMatrix[r][c] = (r == c) ? 1 : 0;
				}
			}			
		}
	}
	public BigMatrix(int rows) {this(rows, rows);}
	public BigMatrix(int rows, int cols, double[] data) {
		bigMatrix = new double[rows][cols];
		if (data.length == rows * cols) {
			int marker = 0;
			for (int r = 0; r < rows; r++) {
				for (int c = 0; c < cols; c++) {
					bigMatrix[r][marker % cols] = data[marker];
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
	public BigMatrix(double[][] values) {bigMatrix = values;}
	public BigMatrix() {bigMatrix = new double[10][10];}
	public int numNonzeroes(double[] arr) {
		int crement = 0;
		for (double i : arr) {
			crement++;
		}
		return crement;
	}

	public static BigMatrix[] readBigMatrixFromFile(String fileName) throws IOException {
		//holds string form of matrix (rows, cols, data...)
		ArrayList<String> matrixLines = new ArrayList<>();
		ArrayList<BigMatrix> matrices = new ArrayList<BigMatrix>();

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
			double[] nums = new double[numStrings.length];
			for(int i = 0; i < numStrings.length; i++) {
				nums[i] = Double.parseDouble(numStrings[i]);
			}
			matrices.add(new BigMatrix((int) nums[0], (int) nums[1], java.util.Arrays.copyOfRange(nums, 2, nums.length)));
		}
		BigMatrix[] finalMatrices = new BigMatrix[matrices.size()];
		return matrices.toArray(finalMatrices);
	}

	public static String toString(double[] xs) {
		String stringified = "{ ";
		for (int i = 0; i < xs.length; i++) {
			stringified += String.format("%2f", xs[i]) + (i != xs.length - 1 ? ", " : " ");
		}
		return stringified + "}";
	}

	public static String toString(double[] xs, int padSpace) {
		String stringified = "{ ";
		String formatString = "%+" + padSpace + ".5f";
		for (int i = 0; i < xs.length; i++) {
			stringified += String.format(formatString, xs[i]) + (i != xs.length - 1 ? ", " : " ");
		}
		return stringified + "}";
	}
	
	public int largestNumSize() {
		int largest = 0;
		for (double[] row : bigMatrix) {
			for (double i : row) {
				//int iLen = Double.toString(i).length();
				int iLen = String.format("%+.2f", i).length();
				largest = iLen > largest ? iLen : largest;
			}
		}
		//adding 1 b/c of 1 precision points after decimal
		return largest;
	}

	public String toString() {
		String stringified = "{ ";
		int largestNumSize = largestNumSize();

		for(int i = 0; i < bigMatrix.length; i++) {
			stringified += (i == 0 ? "" : "  ") + toString(bigMatrix[i], largestNumSize) + (i != bigMatrix.length - 1 ? "\n" : " }");
		}
		
		return stringified;
	}


	//caution - this only works predictably if rows are of equal length b/c it sorts by leading index, regardless of total length
	class RowComparator implements java.util.Comparator<double[]> {
		public RowComparator () {}
		public int compare(double[] a, double[] b) {
			int aLeading = getLeadingPosition(a);
			int bLeading = getLeadingPosition(b);
			
	
			return aLeading - bLeading;
		}
	}

	//need to update. shouldnt intake matrix
	public void flipRows(int flipee, int flipend, BigMatrix m) {
		double[] oldFlipee = m.getRow(flipee).clone();
		m.setRow(flipee, m.getRow(flipend));
		m.setRow(flipend, oldFlipee);
	}

	public static double[] addRows(double[] firstRow, double firstScale, double[] secondRow, double secondScale) {
		double[] first = scaleRow(firstRow, firstScale);
		double[] second = scaleRow(secondRow, secondScale);

		double[] last = new double[first.length];

		for (int i = 0; i < first.length; i++) {
			last[i] = first[i] + second[i];
		}

		return last;
	}

	public static double[] addRows2(double[] a, double as, double[] b, double bs) {
		double[] f = new double[a.length];
		double[] g = new double[b.length];
		double[] h = new double[b.length];
		for (int x = 0; x < a.length; x++) {
			f[x] = a[x] * as;
		}

		for (int y = 0; y < b.length; y++) {
			g[y] = b[y] * bs;
		}

		for (int z = 0; z < b.length; z++) {
			h[z] = f[z] + g[z];
		}

		return h;
	}

	public void addRowsSF(int firstRow, double firstScale, int secondRow, double secondScale) {
		bigMatrix[firstRow] = addRows(bigMatrix[firstRow], firstScale, bigMatrix[secondRow], secondScale);
	}

	public static double[] scaleRow(double[] row, double scaleBy) {
		double[] scaledRow = row;

		for(int i = 0; i < row.length; i++) {
			scaledRow[i] = row[i] * scaleBy;
		}

		return scaledRow;
	}

	public void scaleRowSF(int row, double scaleBy) {
		bigMatrix[row] = scaleRow(bigMatrix[row], scaleBy);
	}

	public double[][] getbigMatrix() {
		return bigMatrix;
	}

	public double[] getRow(int r) {
		return bigMatrix[r];
	}

	public void setRow(int dex, double[] r) {
		bigMatrix[dex] = r;
	}

	public void switchRows(int first, int second) {
		double[] firstBackup = getRow(first);

		setRow(first, getRow(second));
		setRow(second, firstBackup);
	}

	/*
	takes a row
	returns int[2] where int[0] is leading value, int[1] is index
	*/
	public static double[] getLeading(double[] row) {
		double[] leading = {0, row.length - 1};
		for (int i = 0; i < row.length; i++) {
			if (row[i] == 0) {
				continue;
			} else {
				leading[0] = row[i];
				leading[1] = i;
				return leading;
			}
		}
		return leading;
	}

	public static int getLeadingPosition(double[] row) {
		return (int) getLeading(row)[1];
	}

	public static double getLeadingNum(double[] row) {
		return getLeading(row)[0];
	}

	public static double[] getLeadingNums(double[][] matrix) {
		double[] nums = new double[matrix.length];
		for (int i = 0 ; i < matrix.length; i++) {
			nums[i] = getLeadingNum(matrix[i]);
		}
		return nums;
	}

	//sorts bigMatrix so leftmost leading value is on top
	public void sortRows() {
		java.util.Arrays.sort(bigMatrix, new RowComparator());

		int currentPosition = getLeadingPosition(bigMatrix[0]);
		int currentPositionHighest = 0;

		for (int i = 0; i < bigMatrix.length; i++) {
			int pivotPos = getLeadingPosition(bigMatrix[i]);
			double pivotVal = getLeadingNum(bigMatrix[i]);

			if (pivotPos == currentPosition) {
				if (pivotPos == 1) {
					switchRows(i, currentPositionHighest);
				} else if (pivotPos == -1) {
					scaleRowSF(i, -1);
				} else {
					continue;
				}				
			} else {
				currentPosition = pivotPos;
				currentPositionHighest = i;
			}
		}
	}

	private static boolean hasNonOneOrZero(double[] leadingNums) {
		boolean has = false;

		for(double d: leadingNums) {
			if (! (d == 1 | d == 0)) {
				has = true;
			}
		}

		return has;
	}

	public void reduce() {
		sortRows();

		double leadCurrent = 1;
		double leadCume = 1;
		
		//all leading made to 1's

		while (hasNonOneOrZero(getLeadingNums(bigMatrix))) {

			int currentPosition = getLeadingPosition(bigMatrix[0]);
			int currentPositionHighest = 0;

			for (int i = 0; i < bigMatrix.length; i++) {
				int pivotPos = getLeadingPosition(bigMatrix[i]);
				double pivotVal = getLeadingNum(bigMatrix[i]);

				if (pivotVal == 0) {
					//stop loop b/c we've reached bottom
					break;
				}

				if (pivotPos == currentPosition) {
					if (currentPositionHighest != i) {
						bigMatrix[i] = addRows2(bigMatrix[i], leadCurrent, scaleRow(bigMatrix[currentPositionHighest], leadCurrent), -(pivotVal));
					}
				} else {
					currentPosition = pivotPos;
					currentPositionHighest = i;
				}

				if (currentPositionHighest == i) {		
					scaleRowSF(i, 1.0 / pivotVal);
					leadCurrent = pivotVal;

					if (! (leadCume % pivotVal == 0)) {
						leadCume *= pivotVal;
					}
				}
			}	

			sortRows();
		}

		//System.out.println(leadCume);
		sortRows();
	}

	public static double[] modOne(double first, double second) {
		double[] coeffs = {0, 0, 0};

		//because i do the calculations using absolute values, i have to flip coefficients in the end for negative nums
		boolean negateA = first < 0;
		boolean negateB = second < 0;

		int a = (int) java.lang.Math.abs(first);
		int b = (int) java.lang.Math.abs(second);

		//basically a filter for prima facia unModOneAbles
		if (a == b | a == 0 | b == 0 | b % a == 0 | b % 2 == 0 && a % 2 == 0) {
			return coeffs;
		}

		for (int i = 1; i < b; i++) {
			if (((i * a) - 1) % b == 0) {
				coeffs[0] = i;
				coeffs[1] = ((i * a) - 1) / b;
				break;
			} else if (((i * a) + 1) % b == 0) {
				coeffs[0] = i;
				coeffs[1] = ((i * a) + 1) / b;
				break;
			} else {
				continue;
			}
		}

		coeffs[0] *= negateA ? -1 : 1;
		coeffs[1] *= negateB ? -1 : 1;
		
		a *= negateA ? -1 : 1;
		b *= negateB ? -1 : 1;

		coeffs[2] = (coeffs[1] * b) - (coeffs[0] * a);

		return coeffs;
	}
}