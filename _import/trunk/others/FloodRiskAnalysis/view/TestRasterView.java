package view;

/**
 * @author N. Peiler
 */

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferFloat;
import java.awt.image.IndexColorModel;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.awt.image.SampleModel;
import java.awt.image.renderable.ParameterBlock;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.HashMap;

import javax.media.jai.ImageLayout;
import javax.media.jai.JAI;
import javax.media.jai.PlanarImage;
import javax.media.jai.RasterFactory;
import javax.media.jai.RenderedOp;
import javax.media.jai.TiledImage;
import javax.media.jai.iterator.RandomIter;
import javax.media.jai.iterator.RandomIterFactory;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import com.sun.media.jai.widget.DisplayJAI;

public class TestRasterView extends JFrame implements MouseMotionListener,
		ActionListener {

	private JLabel label;

	//private DisplayDEM dd;

	private DisplayFalseColorDEM dfcd;

	private JButton[] lutButtons;

	private String[] labels = { "Red", "Green", "Blue", "Cyan", "Yellow",
			"Magenta", "Gray", "Inverted Gray", "2 Levels", "4 Levels",
			"8 Levels", "16 Levels", "32 Levels", "64 Levels", "128 Levels",
			"Red-Cyan", "Green-Magenta", "Blue-Yellow", "Sin RGB", "Sin RBG",
			"Sin GRB", "Sin GBR", "Sin BRG", "Sin BGR", "Sqrt RGB", "Sqrt RBG",
			"Sqrt GRB", "Sqrt GBR", "Sqrt BRG", "Sqrt BGR", "Hue RGB",
			"Hue RBG", "Hue GRB", "Hue GBR", "Hue BRG", "Hue BGR",
			"Sin RGB (0)", "Sin RBG (0)", "Sin GRB (0)", "Sin GBR (0)",
			"Sin BRG (0)", "Sin BGR (0)", "Sqrt RGB (0)", "Sqrt RBG (0)",
			"Sqrt GRB (0)", "Sqrt GBR (0)", "Sqrt BRG (0)", "Sqrt BGR (0)",
			"Hue RGB (0)", "Hue RBG (0)", "Hue GRB (0)", "Hue GBR (0)",
			"Hue BRG (0)", "Hue BGR (0)", "Red Saw 2", "Red Saw 4",
			"Red Saw 8", "Green Saw 2", "Green Saw 4", "Green Saw 8",
			"Blue Saw 2", "Blue Saw 4", "Blue Saw 8", "Red-Green Saw 2",
			"Red-Green Saw 4", "Red-Green Saw 8", "Red-Blue Saw 2",
			"Red-Blue Saw 4", "Red-Blue Saw 8", "Green-Blue Saw 2",
			"Green-Blue Saw 4", "Green-Blue Saw 8", "Random 256", "Random 32",
			"Random 8" };

	private TestRasterView(PlanarImage image) {
		super();
		setTitle("DisplayJAI");
		getContentPane().setLayout(new BorderLayout());
		// Create an instance of DisplayDEM.
		//dd = new DisplayDEM(image);
		dfcd = new DisplayFalseColorDEM(image);
		// Add to the JFrame's ContentPane an instance of JScrollPane containing
		// the
		// DisplayDEM instance.
		//		 Add a JLabel so we can see the original DEMs values under the mouse
		// cursor
		label = new JLabel("---");
		getContentPane().add(label, BorderLayout.SOUTH);
		//		 Add a whole great bunch of buttons (three columns) in a JPanel.
		JPanel bPanel = new JPanel(new GridLayout(0, 3));
		lutButtons = new JButton[labels.length];
		for (int b = 0; b < labels.length; b++) {
			lutButtons[b] = new JButton(labels[b]);
			lutButtons[b].addActionListener(this);
			bPanel.add(lutButtons[b]);
		}
		// Add the button panel to the content pane of this JFrame.
		getContentPane().add(bPanel, BorderLayout.EAST);
		// Add the event registry.
		dfcd.addMouseMotionListener(this);
		getContentPane().add(new JScrollPane(dfcd), BorderLayout.CENTER);
		// Set the closing operation so the application is finished.
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setSize(400, 400); // adjust the frame size.
		setVisible(true); // show the frame.
	}

	private PlanarImage getSurrogateImage(PlanarImage originalImage) {
		PlanarImage surrogateImage = null;
		double minValue = 0;
		double maxValue = 0;
		ParameterBlock pbMaxMin = new ParameterBlock();
		pbMaxMin.addSource(originalImage);
		RenderedOp extrema = JAI.create("extrema", pbMaxMin);
		// Must get the extrema of all bands !
		double[] allMins = (double[]) extrema.getProperty("minimum");
		double[] allMaxs = (double[]) extrema.getProperty("maximum");
		minValue = allMins[0];
		maxValue = allMaxs[0];
		for (int v = 1; v < allMins.length; v++) {
			if (allMins[v] < minValue)
				minValue = allMins[v];
			if (allMaxs[v] > maxValue)
				maxValue = allMaxs[v];
		}
		// Rescale the image with the parameters
		double[] subtract = new double[1];
		subtract[0] = minValue;
		double[] divide = new double[1];
		divide[0] = 255. / (maxValue - minValue);
		// Now we can rescale the pixels gray levels:
		ParameterBlock pbRescale = new ParameterBlock();
		pbRescale.add(divide);
		pbRescale.add(subtract);
		pbRescale.addSource(originalImage);
		surrogateImage = JAI.create("rescale", pbRescale, null);
		// Let's convert the data type for displaying.
		ParameterBlock pbConvert = new ParameterBlock();
		pbConvert.addSource(surrogateImage);
		pbConvert.add(DataBuffer.TYPE_BYTE);
		surrogateImage = JAI.create("format", pbConvert);
		return surrogateImage;
	}

	/**
	 * This method will answer to button clicked events, and change the LUT on
	 * the image being displayed. I bet there is a shorter way to do this, but
	 * it won't probably be as direct as the below.
	 */
	public void actionPerformed(ActionEvent e) {
		JButton pressed = (JButton) e.getSource();
		if (pressed.getText().equalsIgnoreCase("Red"))
			dfcd.setLUT(LUTs.red());
		else if (pressed.getText().equalsIgnoreCase("Green"))
			dfcd.setLUT(LUTs.green());
		else if (pressed.getText().equalsIgnoreCase("Blue"))
			dfcd.setLUT(LUTs.blue());
		else if (pressed.getText().equalsIgnoreCase("Cyan"))
			dfcd.setLUT(LUTs.cyan());
		else if (pressed.getText().equalsIgnoreCase("Magenta"))
			dfcd.setLUT(LUTs.magenta());
		else if (pressed.getText().equalsIgnoreCase("Yellow"))
			dfcd.setLUT(LUTs.yellow());
		else if (pressed.getText().equalsIgnoreCase("Gray"))
			dfcd.setLUT(LUTs.gray());
		else if (pressed.getText().equalsIgnoreCase("Inverted Gray"))
			dfcd.setLUT(LUTs.inverted_gray());
		else if (pressed.getText().equalsIgnoreCase("2 Levels"))
			dfcd.setLUT(LUTs.two_levels());
		else if (pressed.getText().equalsIgnoreCase("4 Levels"))
			dfcd.setLUT(LUTs.four_levels());
		else if (pressed.getText().equalsIgnoreCase("8 Levels"))
			dfcd.setLUT(LUTs.eight_levels());
		else if (pressed.getText().equalsIgnoreCase("16 Levels"))
			dfcd.setLUT(LUTs.sixteen_levels());
		else if (pressed.getText().equalsIgnoreCase("32 Levels"))
			dfcd.setLUT(LUTs.thirty_two_levels());
		else if (pressed.getText().equalsIgnoreCase("64 Levels"))
			dfcd.setLUT(LUTs.sixty_four_levels());
		else if (pressed.getText().equalsIgnoreCase("128 Levels"))
			dfcd.setLUT(LUTs.hundred_twenty_eight_levels());
		else if (pressed.getText().equalsIgnoreCase("Red-Cyan"))
			dfcd.setLUT(LUTs.red_cyan());
		else if (pressed.getText().equalsIgnoreCase("Green-Magenta"))
			dfcd.setLUT(LUTs.green_magenta());
		else if (pressed.getText().equalsIgnoreCase("Blue-Yellow"))
			dfcd.setLUT(LUTs.blue_yellow());
		else if (pressed.getText().equalsIgnoreCase("Sin RGB"))
			dfcd.setLUT(LUTs.sin_rgb());
		else if (pressed.getText().equalsIgnoreCase("Sin RBG"))
			dfcd.setLUT(LUTs.sin_rbg());
		else if (pressed.getText().equalsIgnoreCase("Sin GRB"))
			dfcd.setLUT(LUTs.sin_grb());
		else if (pressed.getText().equalsIgnoreCase("Sin GBR"))
			dfcd.setLUT(LUTs.sin_gbr());
		else if (pressed.getText().equalsIgnoreCase("Sin BRG"))
			dfcd.setLUT(LUTs.sin_brg());
		else if (pressed.getText().equalsIgnoreCase("Sin BGR"))
			dfcd.setLUT(LUTs.sin_bgr());
		else if (pressed.getText().equalsIgnoreCase("Sqrt RGB"))
			dfcd.setLUT(LUTs.sqrt_rgb());
		else if (pressed.getText().equalsIgnoreCase("Sqrt RBG"))
			dfcd.setLUT(LUTs.sqrt_rbg());
		else if (pressed.getText().equalsIgnoreCase("Sqrt GRB"))
			dfcd.setLUT(LUTs.sqrt_grb());
		else if (pressed.getText().equalsIgnoreCase("Sqrt GBR"))
			dfcd.setLUT(LUTs.sqrt_gbr());
		else if (pressed.getText().equalsIgnoreCase("Sqrt BRG"))
			dfcd.setLUT(LUTs.sqrt_brg());
		else if (pressed.getText().equalsIgnoreCase("Sqrt BGR"))
			dfcd.setLUT(LUTs.sqrt_bgr());
		else if (pressed.getText().equalsIgnoreCase("Hue RGB"))
			dfcd.setLUT(LUTs.hue_rgb());
		else if (pressed.getText().equalsIgnoreCase("Hue RBG"))
			dfcd.setLUT(LUTs.hue_rbg());
		else if (pressed.getText().equalsIgnoreCase("Hue GRB"))
			dfcd.setLUT(LUTs.hue_grb());
		else if (pressed.getText().equalsIgnoreCase("Hue GBR"))
			dfcd.setLUT(LUTs.hue_gbr());
		else if (pressed.getText().equalsIgnoreCase("Hue BRG"))
			dfcd.setLUT(LUTs.hue_brg());
		else if (pressed.getText().equalsIgnoreCase("Hue BGR"))
			dfcd.setLUT(LUTs.hue_bgr());
		else if (pressed.getText().equalsIgnoreCase("Sin RGB (0)"))
			dfcd.setLUT(LUTs.sin_rgb_0());
		else if (pressed.getText().equalsIgnoreCase("Sin RBG (0)"))
			dfcd.setLUT(LUTs.sin_rbg_0());
		else if (pressed.getText().equalsIgnoreCase("Sin GRB (0)"))
			dfcd.setLUT(LUTs.sin_grb_0());
		else if (pressed.getText().equalsIgnoreCase("Sin GBR (0)"))
			dfcd.setLUT(LUTs.sin_gbr_0());
		else if (pressed.getText().equalsIgnoreCase("Sin BRG (0)"))
			dfcd.setLUT(LUTs.sin_brg_0());
		else if (pressed.getText().equalsIgnoreCase("Sin BGR (0)"))
			dfcd.setLUT(LUTs.sin_bgr_0());
		else if (pressed.getText().equalsIgnoreCase("Sqrt RGB (0)"))
			dfcd.setLUT(LUTs.sqrt_rgb_0());
		else if (pressed.getText().equalsIgnoreCase("Sqrt RBG (0)"))
			dfcd.setLUT(LUTs.sqrt_rbg_0());
		else if (pressed.getText().equalsIgnoreCase("Sqrt GRB (0)"))
			dfcd.setLUT(LUTs.sqrt_grb_0());
		else if (pressed.getText().equalsIgnoreCase("Sqrt GBR (0)"))
			dfcd.setLUT(LUTs.sqrt_gbr_0());
		else if (pressed.getText().equalsIgnoreCase("Sqrt BRG (0)"))
			dfcd.setLUT(LUTs.sqrt_brg_0());
		else if (pressed.getText().equalsIgnoreCase("Sqrt BGR (0)"))
			dfcd.setLUT(LUTs.sqrt_bgr_0());
		else if (pressed.getText().equalsIgnoreCase("Hue RGB (0)"))
			dfcd.setLUT(LUTs.hue_rgb_0());
		else if (pressed.getText().equalsIgnoreCase("Hue RBG (0)"))
			dfcd.setLUT(LUTs.hue_rbg_0());
		else if (pressed.getText().equalsIgnoreCase("Hue GRB (0)"))
			dfcd.setLUT(LUTs.hue_grb_0());
		else if (pressed.getText().equalsIgnoreCase("Hue GBR (0)"))
			dfcd.setLUT(LUTs.hue_gbr_0());
		else if (pressed.getText().equalsIgnoreCase("Hue BRG (0)"))
			dfcd.setLUT(LUTs.hue_brg_0());
		else if (pressed.getText().equalsIgnoreCase("Hue BGR (0)"))
			dfcd.setLUT(LUTs.hue_bgr_0());
		else if (pressed.getText().equalsIgnoreCase("Red Saw 2"))
			dfcd.setLUT(LUTs.red_saw_2());
		else if (pressed.getText().equalsIgnoreCase("Red Saw 4"))
			dfcd.setLUT(LUTs.red_saw_4());
		else if (pressed.getText().equalsIgnoreCase("Red Saw 8"))
			dfcd.setLUT(LUTs.red_saw_8());
		else if (pressed.getText().equalsIgnoreCase("Green Saw 2"))
			dfcd.setLUT(LUTs.green_saw_2());
		else if (pressed.getText().equalsIgnoreCase("Green Saw 4"))
			dfcd.setLUT(LUTs.green_saw_4());
		else if (pressed.getText().equalsIgnoreCase("Green Saw 8"))
			dfcd.setLUT(LUTs.green_saw_8());
		else if (pressed.getText().equalsIgnoreCase("Blue Saw 2"))
			dfcd.setLUT(LUTs.blue_saw_2());
		else if (pressed.getText().equalsIgnoreCase("Blue Saw 4"))
			dfcd.setLUT(LUTs.blue_saw_4());
		else if (pressed.getText().equalsIgnoreCase("Blue Saw 8"))
			dfcd.setLUT(LUTs.blue_saw_8());
		else if (pressed.getText().equalsIgnoreCase("Red-Green Saw 2"))
			dfcd.setLUT(LUTs.red_green_saw_2());
		else if (pressed.getText().equalsIgnoreCase("Red-Green Saw 4"))
			dfcd.setLUT(LUTs.red_green_saw_4());
		else if (pressed.getText().equalsIgnoreCase("Red-Green Saw 8"))
			dfcd.setLUT(LUTs.red_green_saw_8());
		else if (pressed.getText().equalsIgnoreCase("Red-Blue Saw 2"))
			dfcd.setLUT(LUTs.red_blue_saw_2());
		else if (pressed.getText().equalsIgnoreCase("Red-Blue Saw 4"))
			dfcd.setLUT(LUTs.red_blue_saw_4());
		else if (pressed.getText().equalsIgnoreCase("Red-Blue Saw 8"))
			dfcd.setLUT(LUTs.red_blue_saw_8());
		else if (pressed.getText().equalsIgnoreCase("Green-Blue Saw 2"))
			dfcd.setLUT(LUTs.green_blue_saw_2());
		else if (pressed.getText().equalsIgnoreCase("Green-Blue Saw 4"))
			dfcd.setLUT(LUTs.green_blue_saw_4());
		else if (pressed.getText().equalsIgnoreCase("Green-Blue Saw 8"))
			dfcd.setLUT(LUTs.green_blue_saw_8());
		else if (pressed.getText().equalsIgnoreCase("Random 256"))
			dfcd.setLUT(LUTs.random_256());
		else if (pressed.getText().equalsIgnoreCase("Random 32"))
			dfcd.setLUT(LUTs.random_32());
		else if (pressed.getText().equalsIgnoreCase("Random 8"))
			dfcd.setLUT(LUTs.random_8());
	}

	/**
	 * The application entry point.
	 * 
	 * @param args
	 *            the command line arguments.
	 */
	public static void main(String[] args) {
		int width = 1024; // Dimensions of the image.
		int height = 1024;
		// The original image data will be stored on this array.
		float[][] imageData = new float[width][height];
		// We'll fill the array with a degradé pattern.
		for (int w = 0; w < width; w++)
			for (int h = 0; h < height; h++) {
				imageData[w][h] = (float) (Math.sqrt(w + h));
			}
		// Now we have a gray scale image in a two-dimensional array.
		// Convert the 2-dim array a single array.
		float[] imageDataSingleArray = new float[width * height];
		int count = 0;
		// It is important to have the height/width order here !
		for (int h = 0; h < height; h++)
			for (int w = 0; w < width; w++) {
				imageDataSingleArray[count++] = imageData[w][h];
			}
		// Create a Data Buffer from the values on the single image array.
		DataBufferFloat dbuffer = new DataBufferFloat(imageDataSingleArray,
				width * height);
		// Create a float data sample model.
		SampleModel sampleModel = RasterFactory.createBandedSampleModel(
				DataBuffer.TYPE_FLOAT, width, height, 1);
		// Create a compatible ColorModel.
		ColorModel colorModel = PlanarImage.createColorModel(sampleModel);
		// Create a WritableRaster.
		Raster raster = RasterFactory.createWritableRaster(sampleModel,
				dbuffer, new Point(0, 0));
		// Create a TiledImage using the float SampleModel.
		TiledImage tiledImage = new TiledImage(0, 0, width, height, 0, 0,
				sampleModel, colorModel);
		// Set the data of the tiled image to be the raster.
		tiledImage.setData(raster);
		// Save the image on a file.
		//JAI.create("filestore", tiledImage, "floatpattern.tif", "TIFF");

		Raster raster1 = getRaster(new File("annualDamage.asc"));
		ColorModel colorModel1 = PlanarImage.createColorModel(raster1
				.getSampleModel());
		TiledImage tiledImage1 = new TiledImage(0, 0, raster1.getWidth(),
				raster1.getHeight(), 0, 0, raster1.getSampleModel(),
				colorModel1);
		tiledImage1.setData(raster1);

		new TestRasterView(tiledImage1);
	}

	private static Raster getRaster(File asciiRasterFile) {
		int nCols = 0;
		int nRows = 0;
		float[] imageDataArray = null;
		try {
			BufferedReader br = new BufferedReader(new FileReader(
					asciiRasterFile));
			String[] data = new String[6];
			String line;
			for (int i = 0; i < data.length; i++) {
				line = br.readLine();
				int index = line.indexOf(" ");
				String subString = line.substring(index);
				data[i] = subString.trim();
				System.out.println(data[i]);
			}
			nCols = new Integer(data[0]).intValue();
			nRows = new Integer(data[1]).intValue();
			imageDataArray = new float[nCols * nRows];
			String nodata = data[5];
			int x = 0;
			while ((line = br.readLine()) != null) {
				//rangeData.append(line);
				String[] dataAsString = line.split(" ");
				//System.out.println("...");
				for (int i = 0; i < dataAsString.length; i++) {
					if (dataAsString[i].equals(nodata)) {
						imageDataArray[i + (x * nCols)] = -1;
					} else {
						imageDataArray[i + (x * nCols)] = (new Float(
								dataAsString[i])).floatValue();
					}
				}
				x = x + 1;
			}
		} catch (Exception e) {
			System.out.println(e);
		}
		int size = nCols * nRows;
		DataBufferFloat dataBuffer = new DataBufferFloat(imageDataArray, size);
		SampleModel sampleModel = RasterFactory.createBandedSampleModel(
				DataBuffer.TYPE_FLOAT, nCols, nRows, 1);
		Point origin = new Point(0, 0);
		Raster raster = RasterFactory.createWritableRaster(sampleModel,
				dataBuffer, origin);
		return raster;
	}

	/**
	 * This method will not do anything - it is here just to keep the
	 * MouseMotionListener interface happy.
	 */
	public void mouseDragged(MouseEvent e) {
	}

	/**
	 * This method will be executed when the mouse is moved over the extended
	 * display class.
	 * 
	 * @param e
	 *            the mouse event
	 */
	public void mouseMoved(MouseEvent e) {
		label.setText(dfcd.getPixelInfo()); // just update the label with the
		// DisplaJAI instance info.
	}
}

class DisplayDEM extends DisplayJAI implements MouseMotionListener {
	protected StringBuffer pixelInfo; // the pixel information (formatted as a

	// StringBuffer).
	protected double[] dpixel; // the pixel information as an array of doubles.

	protected RandomIter readIterator; // a RandomIter that allow us to get the

	// data of a single pixel.
	protected PlanarImage surrogateImage; // the surrogate byte image

	protected int width, height; // the dimensions of the image

	protected double minValue, maxValue; // the range of the image values.

	/**
	 * The constructor of the class, which creates the arrays and instances
	 * needed to obtain the image data and registers the class to listen to
	 * mouse motion events.
	 * 
	 * @param image
	 *            a RenderedImage for display
	 */
	public DisplayDEM(RenderedImage image) {
		// Create the iterator
		readIterator = RandomIterFactory.create(image, null);
		// Get some facts about the image
		width = image.getWidth();
		height = image.getHeight();
		// Create an array to receive the pixels values with the appropriate
		// number
		// of bands
		dpixel = new double[image.getSampleModel().getNumBands()];
		// Which are the max and min of the image ? We need to know to create
		// the
		// surrogate image.
		// Let's use the extrema operator to get them.
		ParameterBlock pbMaxMin = new ParameterBlock();
		pbMaxMin.addSource(image);
		RenderedOp extrema = JAI.create("extrema", pbMaxMin);
		// Must get the extrema of all bands !
		double[] allMins = (double[]) extrema.getProperty("minimum");
		double[] allMaxs = (double[]) extrema.getProperty("maximum");
		minValue = allMins[0];
		maxValue = allMaxs[0];
		for (int v = 1; v < allMins.length; v++) {
			if (allMins[v] < minValue)
				minValue = allMins[v];
			if (allMaxs[v] > maxValue)
				maxValue = allMaxs[v];
		}
		// Rescale the image with the parameters
		double[] subtract = new double[1];
		subtract[0] = minValue;
		double[] divide = new double[1];
		divide[0] = 255. / (maxValue - minValue);
		// Now we can rescale the pixels gray levels:
		ParameterBlock pbRescale = new ParameterBlock();
		pbRescale.add(divide);
		pbRescale.add(subtract);
		pbRescale.addSource(image);
		surrogateImage = (PlanarImage) JAI.create("rescale", pbRescale, null);
		// Let's convert the data type for displaying.
		ParameterBlock pbConvert = new ParameterBlock();
		pbConvert.addSource(surrogateImage);
		pbConvert.add(DataBuffer.TYPE_BYTE);
		surrogateImage = JAI.create("format", pbConvert);
		set(surrogateImage);
		// Create the StringBuffer instance for the pixel information.
		pixelInfo = new StringBuffer(50);
		// Registers the mouse motion listener.
		addMouseMotionListener(this);
	}

	/**
	 * This method does not do anything, it is here just to keep the
	 * MouseMotionListener interface happy.
	 * 
	 * @param me
	 *            the mouse event that caused the execution of this method.
	 */
	public void mouseDragged(MouseEvent e) {
	}

	/**
	 * This method will be called when the mouse is moved over the image being
	 * displayed.
	 * 
	 * @param me
	 *            the mouse event that caused the execution of this method.
	 */
	public void mouseMoved(MouseEvent me) {
		pixelInfo.setLength(0); // clear the StringBuffer
		int x = me.getX();
		int y = me.getY();
		if ((x >= width) || (y >= height)) // Avoid exceptions, consider only
		{ // pixels within image bounds.
			pixelInfo.append("No data!");
			return;
		}
		pixelInfo.setLength(0); // clear the StringBuffer
		pixelInfo.append("(DEM data) " + x + "," + y + ": ");
		readIterator.getPixel(x, y, dpixel); // read the pixel
		for (int b = 0; b < dpixel.length; b++)
			pixelInfo.append(dpixel[b] + ","); // append to the StringBuffer
		pixelInfo = pixelInfo.deleteCharAt(pixelInfo.length() - 1); // erase
		// comma
	} // end of method mouseMoved

	/**
	 * This method allows external classes access to the pixel info which was
	 * obtained in the mouseMoved method.
	 * 
	 * @return the pixel information, formatted as a string
	 */
	public String getPixelInfo() {
		return pixelInfo.toString();
	}

	/**
	 * This method returns the minimum value of the image data.
	 * 
	 * @return the minimum data value of the image data.
	 */
	public double getMinValue() {
		return minValue;
	}

	/**
	 * This method returns the maximum value of the image data.
	 * 
	 * @return the maximum data value of the image data.
	 */
	public double getMaxValue() {
		return maxValue;
	}

}

class DisplayFalseColorDEM extends DisplayDEM {
	/**
	 * The constructor for the class, which simply calls the constructor for its
	 * ancestral.
	 */
	public DisplayFalseColorDEM(RenderedImage image) {
		super(image);
	}

	/**
	 * This method decodes and sets a LUT for the displayed image, using
	 * different sample and color models.
	 */
	public void setLUT(short[][] colors) {
		// We need to reformat the surrogate image as a 3-bands one.
		// Let's change its SampleModel through the format operator.
		// First we get the image's sample model and create a very similar
		// sample model, but with three bands instead of one.
		SampleModel sampleModel = surrogateImage.getSampleModel();
		SampleModel newSampleModel = RasterFactory.createBandedSampleModel(
				DataBuffer.TYPE_BYTE, sampleModel.getWidth(), sampleModel
						.getHeight(), 3); // three bands now
		// Now we convert the color array (which data type is short) into three
		// separate byte arrays. We always assume that the LUT has 256 entries.
		byte[] reds = new byte[256];
		byte[] greens = new byte[256];
		byte[] blues = new byte[256];
		for (int i = 0; i < 256; i++) {
			reds[i] = (byte) colors[i][0];
			greens[i] = (byte) colors[i][1];
			blues[i] = (byte) colors[i][2];
		}
		// Create an IndexColorModel using the arrays above.
		ColorModel colorModel = new IndexColorModel(8, 256, reds, greens, blues);
		// To change the color model of the surrogate image, we need to create a
		// new
		// image layout based on the image, and change the layout's color model.
		ImageLayout layout = new ImageLayout(surrogateImage);
		layout.setColorModel(colorModel);
		// In order to change the image layout we need to set its rendering
		// hints.
		HashMap map = new HashMap();
		map.put(JAI.KEY_IMAGE_LAYOUT, layout);
		RenderingHints hints = new RenderingHints(map);
		// Reformat the image using the above hints.
		ParameterBlock pb = new ParameterBlock();
		pb.addSource(surrogateImage);
		// We don't really want to change the original surrogate image...
		PlanarImage newSurrogateImage = JAI.create("format", pb, hints);
		// Set the new, LUT-applied image.
		set(newSurrogateImage);
	}

}