package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.FlowLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferFloat;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.awt.image.SampleModel;
import java.awt.image.renderable.ParameterBlock;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.math.BigDecimal;
import java.util.Vector;

import javax.imageio.ImageIO;
import javax.media.jai.Interpolation;
import javax.media.jai.JAI;
import javax.media.jai.PlanarImage;
import javax.media.jai.RasterFactory;
import javax.media.jai.TiledImage;
import javax.media.jai.iterator.RandomIter;
import javax.media.jai.iterator.RandomIterFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;

import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;

import tools.Interval;
import tools.Number;

import com.sun.media.jai.widget.DisplayJAI;

import converter.ArcGridConverter;

/**
 * JInternalFrame for displaying RectifiedGridCoverages
 * 
 * @author N. Peiler
 *  
 */
public class GridRasterView extends JInternalFrame implements ActionListener,
		MouseMotionListener {

	/**
	 * comboBox with AsciiGrid-Files, which are stored in the damageDirectory of
	 * the project
	 */
	JComboBox cb_grids;

	/**
	 * comboBox for choosing the number of categories for the classification of
	 * the rangeSetData (possible 1...10)
	 */
	JComboBox cb_categories;

	/**
	 * comboBox for choosing the scaling factor of the view (possible
	 * 0,25...5,0)
	 */
	JComboBox cb_scales;

	/**
	 * button for saving image as JPG
	 */
	JButton bt_saveJpg;

	/**
	 * label for displaying grid-data
	 */
	JLabel label;

	/**
	 * Panel for displaying a RectifiedGridCoverage
	 */
	ImagePanel imagePanel;

	/**
	 * the MenuHandler of the FloodRiskAnalysisView
	 */
	MenuHandler menuHandler = null;

	/**
	 * constructs a GridRasterView
	 * 
	 * @param menuHandler
	 */
	public GridRasterView(MenuHandler menuHandler) {
		super("RasterView", true, true, true, true);
		setBounds(0, 0, 500, 300);
		this.menuHandler = menuHandler;
		File damageDir = new File(menuHandler.workingDir + "\\Damage");
		initGUI(damageDir);
		show();
	}

	private void initGUI(File damageDir) {
		//TopToolBar
		File[] grids = damageDir.listFiles(new GridFilter());
		cb_grids = new JComboBox(grids);
		cb_grids.setRenderer(new GridCellRenderer());
		cb_grids.addActionListener(this);
		cb_grids.setActionCommand("cb_grids changed");
		JToolBar topToolBar = new JToolBar();
		topToolBar.add(cb_grids);

		//BottomToolBar
		Integer[] categories = { new Integer(1), new Integer(2),
				new Integer(3), new Integer(4), new Integer(5), new Integer(6),
				new Integer(7), new Integer(8), new Integer(9), new Integer(10) };

		Float[] scales = { new Float(0.25), new Float(0.5), new Float(0.75),
				new Float(1), new Float(2), new Float(3), new Float(4),
				new Float(5) };
		cb_scales = new JComboBox(scales);
		cb_scales.setSelectedItem(new Float(1));
		cb_scales.addActionListener(this);
		cb_scales.setActionCommand("scales changed");
		JLabel l_scales = new JLabel("Scale: ");
		bt_saveJpg = new JButton("Save as Jpg");
		bt_saveJpg.addActionListener(this);
		label = new JLabel("---");
		cb_categories = new JComboBox(categories);
		cb_categories.addActionListener(this);
		cb_categories.setActionCommand("cb_categories changed");
		JLabel l_categories = new JLabel("Categories: ");
		JToolBar bottomToolBar = new JToolBar();
		bottomToolBar.setLayout(new FlowLayout(FlowLayout.LEFT, 10, 5));
		bottomToolBar.add(label);
		bottomToolBar.add(l_categories);
		bottomToolBar.add(cb_categories);
		bottomToolBar.add(l_scales);
		bottomToolBar.add(cb_scales);
		bottomToolBar.add(bt_saveJpg);

		//imagePanel
		imagePanel = new ImagePanel(null);
		JScrollPane scrollPane = new JScrollPane(imagePanel);
		imagePanel.addMouseMotionListener(this);

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(topToolBar, BorderLayout.NORTH);
		getContentPane().add(bottomToolBar, BorderLayout.SOUTH);
		getContentPane().add(scrollPane, BorderLayout.CENTER);
	}

	/**
	 * creates a Raster-Object of the raster stored in the given file
	 * 
	 * @param asciiRasterFile
	 * @return Raster
	 */
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

	public void mouseDragged(MouseEvent e) {
    // nothing
	}

	/**
	 * This method will be executed when the mouse is moved over the extended
	 * display class.
	 * 
	 * @param e
	 *            the mouse event
	 */
	public void mouseMoved(MouseEvent e) {
		label.setText(imagePanel.getPixelInfo()); // just update the label with
		// the
		// DisplaJAI instance info.
	}

	// ActionListener:
	public void actionPerformed(ActionEvent e) {
		String action = e.getActionCommand();
		final Component gp = getGlassPane();
		gp.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		gp.addMouseListener(new java.awt.event.MouseAdapter() {
      // nothing
		});
		if ("cb_grids changed".equals(action)) {
			int index = cb_grids.getItemCount();
			if (index > 0) {
				new Thread() {
					public void run() {
						File gridFile = (File) cb_grids.getSelectedItem();
						ArcGridConverter gridConverter = new ArcGridConverter();
						RectifiedGridCoverage gridCoverage = gridConverter
								.importGridArc(gridFile);
						imagePanel.updateRaster(gridCoverage);
						gp.setVisible(false);
					}
				}.start();
				gp.setVisible(true);
			}
		}
		if ("cb_categories changed".equals(action)) {
			new Thread() {
				public void run() {
					Integer numCategories = (Integer) cb_categories
							.getSelectedItem();
					imagePanel.updateRaster(numCategories.intValue());
					gp.setVisible(false);
				}
			}.start();
			gp.setVisible(true);
			cb_scales.setSelectedItem(new Float(1));
		}

		if ("scales changed".equals(action)) {
			Float scale = (Float) cb_scales.getSelectedItem();
			imagePanel.updateRaster(scale.floatValue());
		}

		if ("Save as Jpg".equals(action)) {
			final String fileName = menuHandler.getFileName(false,
					"Save as...", JFileChooser.FILES_ONLY);
			if (fileName != null) {
				new Thread() {
					public void run() {
						imagePanel.saveAsJpg(new File(fileName));
						gp.setVisible(false);
					}
				}.start();
				gp.setVisible(true);
			} else {
				LogView.println("...canceled");
			}
		}
	}

	class ImagePanel extends DisplayJAI implements MouseMotionListener {

		RectifiedGridCoverage m_grid = null;

		PlanarImage m_surrogateImage = null;

		PlanarImage originalImage = null;

		StringBuffer pixelInfo; // the pixel information (formatted as a

		// StringBuffer).

		double[] dpixel; // the pixel information as an array of doubles.

		RandomIter readIterator; // a RandomIter that allow us to get the data

		// of a single pixel.

		int width, height; // the dimensions of the image

		/**
		 * scaling factor for scaling the image
		 */
		float m_scale = 1;

		/**
		 * number of Categories for the classification of the rangeSetData
		 */
		int m_nCategories = 1;

		/**
		 * Vector with intervals for the classification of the rangeSetData
		 */
		Vector intervals = null;

		/**
		 * Array of graded red colors corresponding to the Vector of intervals
		 */
		Color[] colors = null;

		/**
		 * min. value of rangeSetData
		 */
		double minValue = 0;

		/**
		 * min. value of rangeSetData
		 */
		double maxValue = 0;

		ImagePanel(RectifiedGridCoverage grid) {
			m_grid = grid;
			initializeColors();
			//updateColors(nCategories);
			this.setBackground(Color.white);
			if (grid != null) {
				minValue = grid.getRangeSet().getMinValue();
				maxValue = grid.getRangeSet().getMaxValue();
				changeIntervals(m_nCategories);
				Raster[] rasters = getRasters(grid);
				originalImage = getPlanarImage(rasters[0]);
				//Create the iterator
				readIterator = RandomIterFactory.create(originalImage, null);
				//Get some facts about the image
				width = originalImage.getWidth();
				height = originalImage.getHeight();
				// Create an array to receive the pixels values with the
				// appropriate number of bands
				dpixel = new double[originalImage.getSampleModel()
						.getNumBands()];
				m_surrogateImage = getSurrogateImage(rasters[1]);
				set(m_surrogateImage);
			}
			// Create the StringBuffer instance for the pixel information.
			pixelInfo = new StringBuffer(50);
			// Registers the mouse motion listener.
			addMouseMotionListener(this);
		}

		public void mouseDragged(MouseEvent e) {
      // nothing
		}

		/**
		 * This method will be called when the mouse is moved over the image
		 * being displayed.
		 * 
		 * @param me
		 *            the mouse event that caused the execution of this method.
		 */
		public void mouseMoved(MouseEvent me) {
			pixelInfo.setLength(0); // clear the StringBuffer
			int x = me.getX();
			int y = me.getY();
			if ((x >= width) || (y >= height)) // Avoid exceptions, consider
			// only pixels within image
			// bounds.
			{
				pixelInfo.append("---");
				return;
			}
			if (m_scale != 1) // Not available when value for scaling is not 1
			{
				pixelInfo.append("---");
				return;
			}
			pixelInfo.setLength(0); // clear the StringBuffer
			pixelInfo.append(x + "," + y + ": ");
			readIterator.getPixel(x, y, dpixel); // read the pixel
			for (int b = 0; b < dpixel.length; b++)
				pixelInfo.append(Number.round(dpixel[b], 4,
						BigDecimal.ROUND_HALF_EVEN)
						+ ","); // append to the StringBuffer
			pixelInfo = pixelInfo.deleteCharAt(pixelInfo.length() - 1); // erase
			// comma
		} // end of method mouseMoved

		/**
		 * This method allows external classes access to the pixel info which
		 * was obtained in the mouseMoved method.
		 * 
		 * @return the pixel information, formatted as a string
		 */
		public String getPixelInfo() {
			return pixelInfo.toString();
		}

		private void initializeColors() {
			Color baseColor = Color.RED;
			colors = new Color[10];
			// graded red from lightred to darkred
			colors[0] = new Color(254, 210, 210);
			colors[1] = new Color(255, 180, 180);
			colors[2] = new Color(255, 150, 150);
			colors[3] = new Color(254, 90, 90);
			colors[4] = baseColor;
			colors[5] = new Color(224, 0, 0);
			colors[6] = new Color(199, 0, 0);
			colors[7] = new Color(149, 0, 0);
			colors[8] = new Color(124, 0, 0);
			colors[9] = new Color(99, 0, 0);
		}

		private void updateColors(int nCategories) {
			colors = new Color[nCategories];
			if ((nCategories % 2) == 0) {
				double colorStep = 0.7 / (nCategories / 2);
				for (int i = 0; i < nCategories / 2; i++) {
					float h = 0;
					float s1 = new Double(0.3 + i * colorStep).floatValue();
					float s2 = 0;
					float b1 = 0;
					float b2 = new Double(0.3 + i * colorStep).floatValue();
					colors[i] = Color.getHSBColor(h, s1, b1);
					colors[nCategories - 1 - i] = Color.getHSBColor(h, s2, b2);
				}
			} else {
				int temp_nCategories = nCategories - 1;
				double colorStep = 0.7 / (temp_nCategories / 2);
				for (int i = 0; i < temp_nCategories / 2; i++) {
					float h = 0;
					float s1 = new Double(0.3 + i * colorStep).floatValue();
					float s2 = 0;
					float b1 = 0;
					float b2 = new Double(0.3 + i * colorStep).floatValue();
					colors[i] = Color.getHSBColor(h, s1, b1);
					colors[nCategories - 1 - i] = Color.getHSBColor(h, s2,
							b2);
				}
				colors[temp_nCategories / 2] = Color.getHSBColor(0f, 1f, 1f);
			}
		}

		/**
		 * get an image with the given Raster-Object
		 * 
		 * @param raster
		 * @return Image
		 */
		private PlanarImage getPlanarImage(Raster raster) {
			ColorModel colorModel = PlanarImage.createColorModel(raster
					.getSampleModel());
			TiledImage tiledImage = new TiledImage(0, 0, raster.getWidth(),
					raster.getHeight(), 0, 0, raster.getSampleModel(),
					colorModel);
			tiledImage.setData(raster);
			return tiledImage;
		}

		/**
		 * creates an original and a surrogate Raster of the given
		 * RectifiedGridCoverage. The surrogate Raster corresponds to the
		 * definied rangesetData classification and colorMapping.
		 * 
		 * @param grid
		 * @return two Rasters (0: original, 1: surrogate)
		 */
		private Raster[] getRasters(RectifiedGridCoverage grid) {
			Raster[] rasters = new Raster[2];
			int nCols = grid.getGridDomain().getNumColumns();
			int nRows = grid.getGridDomain().getNumRows();
			SampleModel surrogateSampleModel = RasterFactory
					.createBandedSampleModel(DataBuffer.TYPE_INT, nCols, nRows,
							3);
			DataBuffer surrogateDataBuffer = surrogateSampleModel
					.createDataBuffer();
			SampleModel originalSampleModel = RasterFactory
					.createBandedSampleModel(DataBuffer.TYPE_DOUBLE, nCols,
							nRows, 1);
			DataBuffer originalDataBuffer = originalSampleModel
					.createDataBuffer();
			Vector rangeSetData = grid.getRangeSet().getRangeSetData();
			for (int i = 0; i < rangeSetData.size(); i++) {
				Vector rangeSetDataRow = (Vector) rangeSetData.get(i);
				for (int j = 0; j < rangeSetDataRow.size(); j++) {
					Color actualColor = Color.WHITE;
					if (rangeSetDataRow.get(j) != null) {
						double actualValue = ((Double) rangeSetDataRow.get(j))
								.doubleValue();
						originalDataBuffer.setElemDouble(0, j + (i * nCols),
								actualValue);
						for (int n = 0; n < intervals.size(); n++) {
							Interval interval = (Interval) intervals.get(n);
							if (interval.contains(actualValue)) {
								actualColor = colors[n];
							}
						}
						int redValue = actualColor.getRed();
						int greenValue = actualColor.getGreen();
						int blueValue = actualColor.getBlue();
						surrogateDataBuffer.setElem(0, j + (i * nCols),
								redValue);
						surrogateDataBuffer.setElem(1, j + (i * nCols),
								greenValue);
						surrogateDataBuffer.setElem(2, j + (i * nCols),
								blueValue);
					} else {
						originalDataBuffer
								.setElemDouble(0, j + (i * nCols), -1);
						int redValue = actualColor.getRed();
						int greenValue = actualColor.getGreen();
						int blueValue = actualColor.getBlue();
						surrogateDataBuffer.setElem(0, j + (i * nCols),
								redValue);
						surrogateDataBuffer.setElem(1, j + (i * nCols),
								greenValue);
						surrogateDataBuffer.setElem(2, j + (i * nCols),
								blueValue);
					}
				}
			}
			Point origin = new Point(0, 0);
			Raster originalRaster = RasterFactory.createWritableRaster(
					originalSampleModel, originalDataBuffer, origin);
			rasters[0] = originalRaster;
			Raster surrogateRaster = RasterFactory.createWritableRaster(
					surrogateSampleModel, surrogateDataBuffer, origin);
			rasters[1] = surrogateRaster;
			return rasters;
		}

		/**
		 * get a surrogate image for displaying with byte values of the given
		 * raster with int values
		 * 
		 * @param surrogateRaster
		 * @return surrogate image
		 */
		private PlanarImage getSurrogateImage(Raster surrogateRaster) {
			PlanarImage surrogateImage = getPlanarImage(surrogateRaster);
			//			 Let's convert the data type for displaying.
			ParameterBlock pbConvert = new ParameterBlock();
			pbConvert.addSource(surrogateImage);
			pbConvert.add(DataBuffer.TYPE_BYTE);
			surrogateImage = JAI.create("format", pbConvert);
			return surrogateImage;
		}

		/**
		 * creates a surrogate raster of the given RectifiedGridCoverage, which
		 * corresponds to the definied rangesetData classification and
		 * colorMapping
		 * 
		 * @param grid
		 * @return surrogate raster
		 */
		private Raster getSurrogateRaster(RectifiedGridCoverage grid) {
			int nCols = grid.getGridDomain().getNumColumns();
			int nRows = grid.getGridDomain().getNumRows();
			//int size = nCols * nRows;
			SampleModel sampleModel = RasterFactory.createBandedSampleModel(
					DataBuffer.TYPE_INT, nCols, nRows, 3);
			DataBuffer dataBuffer = sampleModel.createDataBuffer();
			Vector rangeSetData = grid.getRangeSet().getRangeSetData();
			for (int i = 0; i < rangeSetData.size(); i++) {
				Vector rangeSetDataRow = (Vector) rangeSetData.get(i);
				for (int j = 0; j < rangeSetDataRow.size(); j++) {
					Color actualColor = Color.WHITE;
					if (rangeSetDataRow.get(j) != null) {
						double actualValue = ((Double) rangeSetDataRow.get(j))
								.doubleValue();
						for (int n = 0; n < intervals.size(); n++) {
							Interval interval = (Interval) intervals.get(n);
							if (interval.contains(actualValue)) {
								actualColor = colors[n];
							}
						}
						int redValue = actualColor.getRed();
						int greenValue = actualColor.getGreen();
						int blueValue = actualColor.getBlue();
						dataBuffer.setElem(0, j + (i * nCols), redValue);
						dataBuffer.setElem(1, j + (i * nCols), greenValue);
						dataBuffer.setElem(2, j + (i * nCols), blueValue);
					} else {
						int redValue = actualColor.getRed();
						int greenValue = actualColor.getGreen();
						int blueValue = actualColor.getBlue();
						dataBuffer.setElem(0, j + (i * nCols), redValue);
						dataBuffer.setElem(1, j + (i * nCols), greenValue);
						dataBuffer.setElem(2, j + (i * nCols), blueValue);
					}
				}
			}
			Point origin = new Point(0, 0);
			Raster surrogateRaster = RasterFactory.createWritableRaster(
					sampleModel, dataBuffer, origin);
			return surrogateRaster;
		}

		/**
		 * invokes, when a new Grid is choosen
		 * 
		 * @param grid
		 */
		void updateRaster(RectifiedGridCoverage grid) {
			if (grid != null) {
				m_grid = grid;
				minValue = grid.getRangeSet().getMinValue();
				maxValue = grid.getRangeSet().getMaxValue();
				changeIntervals(m_nCategories);
				Raster[] rasters = getRasters(grid);
				originalImage = getPlanarImage(rasters[0]);
				//Create the iterator
				readIterator = RandomIterFactory.create(originalImage, null);
				//Get some facts about the image
				width = originalImage.getWidth();
				height = originalImage.getHeight();
				// Create an array to receive the pixels values with the
				// appropriate number of bands
				dpixel = new double[originalImage.getSampleModel()
						.getNumBands()];
				m_surrogateImage = getSurrogateImage(rasters[1]);
				set(m_surrogateImage);
			}
		}

		/**
		 * invokes, when the number of categories has changed
		 * 
		 * @param nCategories
		 *            number of categories
		 */
		void updateRaster(int nCategories) {
			if (m_grid != null) {
				changeIntervals(nCategories);
				//updateColors(nCategories);
				Raster surrogateRaster = getSurrogateRaster(m_grid);
				m_surrogateImage = getSurrogateImage(surrogateRaster);
				set(m_surrogateImage);
			}
		}

		/**
		 * invokes, when the scaling factor is changed
		 * 
		 * @param scale
		 *            scaling factor
		 */
		void updateRaster(float scale) {
			if (m_grid != null) {
				this.m_scale = scale;
				ParameterBlock pb1 = new ParameterBlock();
				/*
				 * RenderingHints rh = new RenderingHints(
				 * RenderingHints.KEY_RENDERING,
				 * RenderingHints.VALUE_RENDER_SPEED);
				 */
				pb1.addSource(m_surrogateImage); // The source image
				pb1.add(scale); // The xScale
				pb1.add(scale); // The yScale
				pb1.add(0.0F); // The x translation
				pb1.add(0.0F); // The y translation
				pb1.add(Interpolation
						.getInstance(Interpolation.INTERP_BILINEAR)); // The
				// interpolation
				RenderedImage newSurrogateImage = JAI.create("scale", pb1);
				set(newSurrogateImage);
			}
		}

		/**
		 * calculates the equal intervals for a given number of categories
		 * 
		 * @param nCategories
		 */
		private void changeIntervals(int nCategories) {
			intervals = new Vector();
			//colors = new Vector();
			double intervalSpace = (maxValue - minValue) / nCategories;
			for (int n = 0; n < nCategories; n++) {
				double startValue = minValue + (n * intervalSpace);
				double endValue = startValue + intervalSpace;
				Interval interval;
				if (n == (nCategories - 1)) {
					interval = new Interval(startValue, maxValue);
				} else {
					interval = new Interval(startValue, endValue);
				}
				int x = n + 1;
				LogView.println("Interval "
						+ x
						+ ": "
						+ Number.round(interval.getLowerLimit(), 4,
								BigDecimal.ROUND_HALF_EVEN)
						+ " - "
						+ Number.round(interval.getUpperLimit(), 4,
								BigDecimal.ROUND_HALF_EVEN));
				intervals.add(interval);
				//Color color = getRandomColor();
				//colors.add(color);
			}
			this.m_nCategories = nCategories;
		}

		/**
		 * saves the actual surrogateImage as JPG
		 * 
		 * @param outFile
		 *            file for saving the image
		 */
		void saveAsJpg(File outFile) {
			try {
				ImageIO.write(m_surrogateImage, "JPG", outFile);

			} catch (Exception e) {
				System.out.println(e);
			}
		}

	}

}