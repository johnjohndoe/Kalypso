/*----------------    FILE HEADER  ------------------------------------------
 
This file is part of deegree.
Copyright (C) 2001 by:
EXSE, Department of Geography, University of Bonn
http://www.giub.uni-bonn.de/exse/
lat/lon Fitzke/Fretter/Poth GbR
http://www.lat-lon.de
 
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.
 
This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.
 
You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 
Contact:
 
Andreas Poth
lat/lon Fitzke/Fretter/Poth GbR
Meckenheimer Allee 176
53115 Bonn
Germany
E-Mail: poth@lat-lon.de
 
Jens Fitzke
Department of Geography
University of Bonn
Meckenheimer Allee 166
53115 Bonn
Germany
E-Mail: jens.fitzke@uni-bonn.de
 
 
 ---------------------------------------------------------------------------*/

package org.deegree_impl.tools;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Iterator;

import org.deegree.graphics.Encoders;
import org.deegree.graphics.legend.LegendElement;
import org.deegree.graphics.sld.NamedLayer;
import org.deegree.graphics.sld.Style;
import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree.graphics.sld.UserLayer;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.graphics.legend.LegendException;
import org.deegree.xml.XMLParsingException;
import org.deegree_impl.graphics.legend.LegendFactory;
import org.deegree_impl.graphics.sld.SLDFactory;

/**
 * This executable class is an application, which reads out an sld-document,
 * creates the corresponding legend-elements and saves them as an image.
 * The class can be executed from the console. Details of use can be requested
 * with <tt>java LegendElementCreator --help</tt>
 * <pre>
 usage: java LegendConsole [-f sld-file -d directory
                            -g format -c color -w width -h height -t title]
                           [--version] [--help]

 mandatory arguments:
     -f file     reads the SLD inputfile.
     -d outdir   name of the directory to save the results in.

 optional arguments:
     -g format   graphics format of the output (default=png).
                 possible values are: bmp, gif, jpg, png, tif
     -c color    background-color (default=white)
                 possible values are: TRANSPARENT (if supported by the
                 graphics-format '-g'), black, blue, cyan, dark_gray, gray,
                 green, light_gray, magenta, orange, pink, red, white, yellow
                 or the hexadecimal codes (i.e. #ffffff for white).
     -w width    width (in pixel) of the legendsymbol (default=40).
     -h height   height (in pixel) of the legendsymbol (default=40).
     -t title    optional title for the legend-element. (no default).
                 If more than one word, put the title in "quotation marks".

 information options:
     --help      shows this help.
     --version   shows the version and exits.
  </pre>
 * <hr>  
 * @author <a href="schaefer@lat-lon.de">Axel Schaefer</a>
 */
public class LegendElementCreator {

	String verbose_output = "";

	// optional parameter
	static String format = "PNG";
	static String colorstring = "#ffffff";
	static Color color = Color.WHITE;
	static int width = 40;
	static int height = 40;
	static String title = null;

	public LegendElementCreator(HashMap argsmap) throws LegendException {
		StringBuffer sb = new StringBuffer();
		if (argsmap != null) {

			// get the parameters
			// mandatory parameter
			String sldFile = (String)argsmap.get("-f");
			String outdir = (String)argsmap.get("-d");

			// optional parameter
			if (argsmap.get("-g") != null)
				format = (String)argsmap.get("-g");
			if (argsmap.get("-c") != null)
				colorstring = (String)argsmap.get("-c");

			// BLACK, BLUE, CYAN, DARK_GRAY, GRAY, GREEN, LIGHT_GRAY, MAGENTA, ORANGE, PINK, RED, WHITE, YELLOW
			if (colorstring.equalsIgnoreCase("BLACK"))
				color = Color.BLACK;
			else if (colorstring.equalsIgnoreCase("BLUE"))
				color = Color.BLUE;
			else if (colorstring.equalsIgnoreCase("CYAN"))
				color = Color.CYAN;
			else if (colorstring.equalsIgnoreCase("DARK_GRAY"))
				color = Color.DARK_GRAY;
			else if (colorstring.equalsIgnoreCase("GRAY"))
				color = Color.GRAY;
			else if (colorstring.equalsIgnoreCase("GREEN"))
				color = Color.GREEN;
			else if (colorstring.equalsIgnoreCase("LIGHT_GRAY"))
				color = Color.LIGHT_GRAY;
			else if (colorstring.equalsIgnoreCase("MAGENTA"))
				color = Color.MAGENTA;
			else if (colorstring.equalsIgnoreCase("ORANGE"))
				color = Color.ORANGE;
			else if (colorstring.equalsIgnoreCase("PINK"))
				color = Color.PINK;
			else if (colorstring.equalsIgnoreCase("RED"))
				color = Color.RED;
			else if (colorstring.equalsIgnoreCase("WHITE"))
				color = Color.WHITE;
			else if (colorstring.equalsIgnoreCase("YELLOW"))
				color = Color.YELLOW;
			else if (colorstring.equalsIgnoreCase("TRANSPARENT"))
				color = null;
			else {
				try {
					color = Color.decode(colorstring);
				} catch (NumberFormatException nfe) {
					throw new LegendException("An error occured during the parsing of the requested background-color:\n" + "-c " + colorstring + "\n" + "The given argument can't be converted to a color.\n" + "A possible value may be #ffffff for the color white (with the '#').\n" + "Output image created with standard-color WHITE.");
				}
			}

			if (argsmap.get("-w") != null)
				width = Integer.parseInt((String)argsmap.get("-w"));
			if (argsmap.get("-h") != null)
				height = Integer.parseInt((String)argsmap.get("-h"));
			// ok, if title is 'null'
			title = (String)argsmap.get("-t");

			HashMap stylemap = null;
			try {
				stylemap = loadSLD(sldFile);
			} catch (IOException ioe) {
				throw new LegendException("An error (IOException) occured in processing the SLD-File:\n" + sldFile + "\n" + ioe);
			} catch (XMLParsingException xmlpe) {
				throw new LegendException(xmlpe.getMessage());
				// "An error (XMLParsingException) occured in processing the SLD-File:\n"
				// + sldFile
				// + "\n"
			}

			// output
			LegendFactory lf = new LegendFactory();
			LegendElement le = null;
			BufferedImage buffi = null;

			Iterator iterator = stylemap.entrySet().iterator();
			String filename = null;
			Style style = null;
			int i = 0;
			while (iterator.hasNext()) {
				i++;
				Map.Entry entry = (Map.Entry)iterator.next();
				filename = ((String)entry.getKey()).replace(':', '_');
				style = (Style)entry.getValue();

				try {
					le = lf.createLegendElement(style, width, height, title);
					buffi = le.exportAsImage();
					saveImage(buffi, outdir, filename, format);
					sb.append("- Image " + filename + "." + format + " in " + outdir + " saved.\n");
				} catch (LegendException lex) {
					throw new LegendException("An error (LegendException) occured during the creating\n" + "of the LegendElement " + filename + ":\n" + lex);
				} catch (IOException ioex) {
					throw new LegendException("An error (IOException) occured during the creating/saving\n" + "of the output-image " + filename + ":\n" + ioex);
				} catch (Exception ex) {
					throw new LegendException("A general error (Exception) occured during the creating/saving\n" + "of the output-image " + filename + ":\n" + ex);
				}

			}
		} else {
			throw new LegendException("General error: HashMap is Null.");
		}
		setVerboseOutput(sb.toString());
	}

	/**
	 * 
	 * @return
	 */
	public String getVerboseOutput() {
		return this.verbose_output;
	}
	
	public void setVerboseOutput(String vo) {
		if (vo != null) {
			this.verbose_output = vo;
		}
	}

	/**
	 * the main-method. doing (nearly) the complete work
	 * @param args given parameters
	 */
	public static void main(String[] args) {
		HashMap argsmap = null;

		if (args.length == 0) {
			System.out.println("LegendElementCreator: missing arguments");
			usage(1);
			System.exit(0);

		} else if (args.length == 1) {
			// one parameter given: must be --version or --help or print help
			if (args[0].equals("--version")) {
				System.out.println("LegendElementCreator v1.0\n" + "This file is part of deegree.\n" + "http://deegree.sourceforge.net\n\n" + "Copyright (C) 2001 by:\n" + "EXSE, Department of Geography, University of Bonn\n" + "http://www.giub.uni-bonn.de/exse/\n" + "lat/lon Fitzke/Fretter/Poth GbR\n" + "http://www.lat-lon.de/");
				System.exit(0);
			} else if (args[0].equals("--help")) {
				usage(0);
				System.exit(0);
			} else {
				System.out.println("LegendElementCreator: invalid option: " + args[0]);
				usage(1);
				System.exit(0);
			}
		} else if (args.length >= 2) {
			argsmap = new HashMap();
			for (int i = 0; i < args.length; i = i + 2) {
				argsmap.put(args[i], args[i + 1]);
			}
		}

		if (argsmap != null) {
			if (argsmap.get("-f") == null || argsmap.get("-d") == null) {
				System.out.println("Missing arguments. -f (SLD-filename) and -d (output-directory) are mandatory.");
				usage(1);
				System.exit(0);
			}

			try {
				new LegendElementCreator(argsmap);
			} catch (LegendException e) {
				System.out.println(e);
			}
		}

		// System.out.println("LegendConsole finished. " + i + " image(s) written.");
	}

	/**
	 * loads the sld-document, parses it an returns a HashMap containing the
	 * different styles. 
	 * @param sldFile the file containing the StyledLayerDescriptor
	 * @return HashMap containing the styles of the SLD.
	 * @throws IOException if the SLD-document cant be read/found in the filesystem
	 * @throws XMLParsingException if an error occurs during the parsing of the sld-document
	 */
	static private HashMap loadSLD(String sldFile) throws IOException, XMLParsingException {
		Style[] styles = null;

		FileReader fr = new FileReader(sldFile);
		StyledLayerDescriptor sld = SLDFactory.createSLD(fr);
		fr.close();

		HashMap map = new HashMap();

		// NAMED LAYER
		NamedLayer[] namedlayers = sld.getNamedLayers();
		for (int i = 0; i < namedlayers.length; i++) {
			styles = namedlayers[i].getStyles();
			for (int j = 0; j < styles.length; j++) {
				if (styles[j] instanceof UserStyle) {
					map.put(styles[j].getName(), styles[j]);
				}
			}
		}

		// USER LAYER
		UserLayer[] userLayers = sld.getUserLayers();
		for (int k = 0; k < userLayers.length; k++) {
			styles = userLayers[k].getStyles();
			for (int l = 0; l < styles.length; l++) {
				if (styles[l] instanceof UserStyle) {
					map.put(styles[l].getName(), styles[l]);
				}
			}
		}
		return map;
	}

	/**
	 * prints out helping application-information. 
	 * @param n an abstract integer parameter, which determines which help-information should be given out. 
	 */
	private static void usage(int n) {
		switch (n) {
			case 0 :
				System.out.println(
					"usage: java -classpath .;deegree.jar de.tools.LegendElement "
						+ "                          [-f sld-file -d directory\n"
						+ "                           -g format -c color -w width -h height -t title]\n"
						+ "                          [--version] [--help]\n"
						+ "\n"
						+ "mandatory arguments:\n"
						+ "    -f file     reads the SLD inputfile.\n"
						+ "    -d outdir   name of the directory to save the results in.\n"
						+ "\n"
						+ "optional arguments:\n"
						+ "    -g format   graphics format of the output (default=png).\n"
						+ "                possible values are: bmp, gif, jpg, png, tif\n"
						+ "    -c color    background-color (default=white)\n"
						+ "                possible values are: TRANSPARENT (if supported by the\n"
						+ "                graphics-format '-g'), black, blue, cyan, dark_gray, gray,\n"
						+ "                green, light_gray, magenta, orange, pink, red, white, yellow\n"
						+ "                or the hexadecimal codes (i.e. #ffffff for white).\n"
						+ "    -w width    width (in pixel) of the legendsymbol (default=40).\n"
						+ "    -h height   height (in pixel) of the legendsymbol (default=40).\n"
						+ "    -t title    optional title for the legend-element. (no default).\n"
						+ "                If more than one word, put the title in \"quotation marks\".\n"
						+ "\n"
						+ "information options:\n"
						+ "    --help      shows this help.\n"
						+ "    --version   shows the version and exits.\n");
				break;
			case 1 :
				System.out.println("Try 'java LegendConsole --help' for more information.");
				break;

			default :
				System.out.println("Unknown usage: Try 'java LegendConsole --help' for more information.");
				break;
		}
	}

	/**
	 * saves the resulting buffered Image from org.deegree.graphics.legend as
	 * an image.
	 * @param bi the BufferedImage from org.deegree.graphics.legend.*
	 * @param outdir the output-directory (application-parameter)
	 * @param filename the output-filename (from the styles of the SLD)
	 * @param graphicsformat the output-graphicsformat (application-parameter)
	 * @throws IOException if saving fails.
	 * @throws Exception if the graphic-encoder can't be found.
	 */
	static private void saveImage(BufferedImage bi, String outdir, String filename, String graphicsformat) throws LegendException, IOException, Exception {

		File file = new File(outdir, filename + "." + graphicsformat);
		FileOutputStream fos = new FileOutputStream(file);

		// BufferedImage outbi = new BufferedImage(bi.getWidth(), bi.getHeight(), BufferedImage.TYPE_INT_ARGB);

		if (graphicsformat.equalsIgnoreCase("PNG")) {

			BufferedImage outbi = new BufferedImage(bi.getWidth(), bi.getHeight(), BufferedImage.TYPE_INT_ARGB);
			Graphics g = outbi.getGraphics();
			g.drawImage(bi, 0, 0, color, null);
			Encoders.encodePng(fos, outbi);
		} else if (graphicsformat.equalsIgnoreCase("BMP")) {
			BufferedImage outbi = new BufferedImage(bi.getWidth(), bi.getHeight(), BufferedImage.TYPE_INT_BGR);
			Graphics g = outbi.getGraphics();

			if (colorstring.equalsIgnoreCase("TRANSPARENT")) {
				System.out.println("Requested transparency (-c TRANSPARENT) isn't available for BMP-images.\n" + "Using default background color WHITE.");
				color = Color.WHITE;
			}

			g.drawImage(bi, 0, 0, color, null);
			Encoders.encodeBmp(fos, outbi);
		} else if (graphicsformat.equalsIgnoreCase("GIF")) {
			BufferedImage outbi = new BufferedImage(bi.getWidth(), bi.getHeight(), BufferedImage.TYPE_INT_ARGB);
			Graphics g = outbi.getGraphics();
			g.drawImage(bi, 0, 0, color, null);
			Encoders.encodeGif(fos, outbi);
		} else if (graphicsformat.equalsIgnoreCase("JPEG") || graphicsformat.equalsIgnoreCase("JPG")) {
			BufferedImage outbi = new BufferedImage(bi.getWidth(), bi.getHeight(), BufferedImage.TYPE_INT_RGB);
			Graphics g = outbi.getGraphics();

			if (colorstring.equalsIgnoreCase("TRANSPARENT")) {
				System.out.println("Requested transparency (-c TRANSPARENT) isn't available for JPG-images.\n" + "Using default background color WHITE.");
				color = Color.WHITE;
			}

			g.drawImage(bi, 0, 0, color, null);
			Encoders.encodeJpeg(fos, outbi, 1f);
		} else if (graphicsformat.equalsIgnoreCase("TIFF") || graphicsformat.equalsIgnoreCase("TIF")) {
			BufferedImage outbi = new BufferedImage(bi.getWidth(), bi.getHeight(), BufferedImage.TYPE_INT_ARGB);
			Graphics g = outbi.getGraphics();
			g.drawImage(bi, 0, 0, color, null);
			Encoders.encodeTiff(fos, outbi);
		} else {
			throw new Exception("Can't save output image because no graphic-encoder found for:\n" + "filetype: '" + graphicsformat + "' for file: '" + file + "'");
		}
	}

}