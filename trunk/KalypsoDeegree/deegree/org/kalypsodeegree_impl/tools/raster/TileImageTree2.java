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
package org.deegree_impl.tools.raster;

import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.Reader;
import java.io.StringReader;
import java.text.DecimalFormat;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.StringTokenizer;

import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;

import org.deegree.graphics.Encoders;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Object;
import org.deegree.xml.DOMPrinter;
import org.deegree.xml.XMLTools;
import org.deegree_impl.io.shpapi.ShapeFile;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.StringExtend;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

import com.sun.media.jai.codec.FileSeekableStream;
import com.sun.media.jai.codec.TIFFDirectory;
import com.sun.media.jai.codec.TIFFField;

/**
 * The program creates a pyramidal structure for a geo-referenced image and
 * splits each level of the 'pyramid' into several tiles. The tiles will be
 * arranged into a Quad-Tree datastructure. The description of this
 * datastructure will be stored into a XML-document that is conform to the
 * deegree coverage descriptor format. (see CVDescriptor.xsd). So the result of
 * the tileing can directly be use with the deegree GridCoverage and Web
 * Coverage Service implementations for fast access to large amounts of raster
 * data.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@latlon.de">Andreas Poth </a>
 * @version 27.12.2002
 */
public class TileImageTree2
{
  // main image
  private BufferedImage image = null;

  private ProgressObserver progressObserver = null;

  private String crs = null;

  // layerID and title of the CV layer
  private String imageName = null;

  private String targetDir = null;

  private String targetFormat = null;

  protected double[] targetResolutions = null;

  // main image resolution
  private double resx = 0;

  private double resy = 0;

  private double xmax = 0;

  // main image bounding box
  private double xmin = 0;

  private double ymax = 0;

  private double ymin = 0;

  private float quality = 0;

  private int count = 0;

  private boolean geoTiff = false;

  private HashMap featColl = new HashMap();

  /** Creates a new instance of TileImageTree */
  public TileImageTree2( String imageSource, String targetDir, String targetFormat,
      double[] targetResolutions, int startIndex, float quality, String crs ) throws Exception
  {
    this.targetDir = targetDir;

    File file = new File( targetDir );

    if( !file.exists() )
    {
      file.mkdir();
    }

    this.targetFormat = targetFormat.toLowerCase();
    this.quality = quality;
    this.targetResolutions = targetResolutions;
    this.progressObserver = new ProgressObserver();

    int pos = imageSource.lastIndexOf( '/' );
    this.imageName = imageSource.substring( pos + 1, imageSource.length() );

    this.crs = crs;

    // load the main image
    image = loadImage( imageSource );

    // get the bounding box of the source image by evaluating its world file
    if( !geoTiff )
    {
      readWorldFile( imageSource );
    }
  }

  /**
   * sets a user defined observer for controling the progress of the tileing
   */
  public void setProgressObserver( ProgressObserver progressObserver )
  {
    this.progressObserver = progressObserver;
  }

  /**
   * loads the base image
   */
  private BufferedImage loadImage( String imageSource ) throws Exception
  {
    System.out.println( "reading source image ..." );

    BufferedImage bi = null;
    FileSeekableStream fss = new FileSeekableStream( imageSource );
    RenderedOp rop = JAI.create( "stream", fss );
    bi = rop.getAsBufferedImage();
    fss.close();
    if( imageSource.toUpperCase().endsWith( ".TIFF" )
        || imageSource.toUpperCase().endsWith( ".TIF" ) )
    {
      geoTiff = isGeoTIFFFormat( rop );
      if( geoTiff )
      {
        readBBoxFromGeoTIFF( rop );
      }
    }
    System.out.println( "finished" );
    return bi;
  }

  /**
   * description: the following TIFFKeys count as indicator if a TIFF-File
   * carries GeoTIFF information: ModelPixelScaleTag = 33550 (SoftDesk)
   * ModelTransformationTag = 34264 (JPL Carto Group) ModelTiepointTag = 33922
   * (Intergraph) GeoKeyDirectoryTag = 34735 (SPOT) GeoDoubleParamsTag = 34736
   * (SPOT) GeoAsciiParamsTag = 34737 (SPOT) implementation status: working
   */
  private boolean isGeoTIFFFormat( RenderedOp rop )
  {
    TIFFDirectory tifDir = (TIFFDirectory)rop.getDynamicProperty( "tiff_directory" );
    if( tifDir.getField( 33550 ) == null && tifDir.getField( 34264 ) == null
        && tifDir.getField( 33922 ) == null && tifDir.getField( 34735 ) == null
        && tifDir.getField( 34736 ) == null && tifDir.getField( 34737 ) == null )
    {
      return false;
    }
    else
    {
      return true;
    }
  }

  /**
   * starts the creation of the tiles
   */
  public void createTileImageTree() throws Exception
  {
    for( int i = 0; i < targetResolutions.length; i++ )
    {
      FeatureCollection fc = FeatureFactory.createFeatureCollection( "fc" + targetResolutions[i],
          1000 );
      featColl.put( "fc" + targetResolutions[i], fc );
    }

    Document doc = XMLTools.create();
    Element root = doc.createElement( "CVDescriptor" );
    root.setAttribute( "xmlns", "http://www.deegree.org/gc" );
    root.setAttribute( "xmlns:wcs", "http://www.opengis.net/wcs" );
    doc.appendChild( root );
    addGridCoverageLayer( root, imageName, imageName, crs, xmin, ymin, xmax, ymax, xmin, ymin,
        xmax, ymax, image.getWidth() - 1, image.getHeight() - 1 );

    System.out.println( "creating tiles ..." );

    tile( image, xmin, ymin, xmax, ymax, 0, root );

    for( int i = 0; i < targetResolutions.length; i++ )
    {
      ShapeFile sh = new ShapeFile( targetDir + "/sh" + targetResolutions[i], "rw" );
      sh.writeShape( (FeatureCollection)featColl.get( "fc" + targetResolutions[i] ) );
      sh.close();

    }
    System.out.println( "100%" );
    System.out.println( "finished" );

    FileOutputStream fos = new FileOutputStream( targetDir + "/gvDesc.xml" );
    fos.write( DOMPrinter.nodeToString( doc, "ISO-8859-1" ).getBytes() );
    fos.close();
  }

  /**
   * the method performes the creation of the tiles and the filling of the
   * quadtree XML-document. the method will be call in a recursion for each
   * defined level (scale).
   */
  private void tile( BufferedImage img, double xmin, double ymin, double xmax, double ymax,
      int res, Element parent ) throws Exception
  {
    // break condition
    if( res >= targetResolutions.length )
    {
      return;
    }

    NodeList list = parent.getElementsByTagName( "Level" );
    Element level = null;
    Element dir = null;

    // create level element and directoy if not already exists
    if( ( list == null ) || ( list.getLength() == 0 ) )
    {
      File file = new File( targetDir + "/l" + targetResolutions[res] );
      file.mkdir();

      // create level node
      level = parent.getOwnerDocument().createElement( "Level" );

      if( res == 0 )
      {
        level.setAttribute( "maxScale", "99999999" );
      }
      else
      {
        level.setAttribute( "maxScale", "" + targetResolutions[res - 1] );
      }

      if( res == ( targetResolutions.length - 1 ) )
      {
        level.setAttribute( "minScale", "0" );
      }
      else
      {
        level.setAttribute( "minScale", "" + targetResolutions[res] );
      }

      parent.appendChild( level );
      dir = addDirectory( 2, 2, targetDir + "/l" + targetResolutions[res], ( xmax - xmin ) / 2d,
          ( ymax - ymin ) / 2d, level );
    }
    else
    {
      level = XMLTools.getFirstElement( parent );
      dir = XMLTools.getFirstElement( level );
    }

    BufferedImage im = null;
    double xmin_ = 0;
    double ymin_ = 0;
    double xmax_ = 0;
    double ymax_ = 0;
    // calculate half of tile width and height to get tile (quarter) coordinates
    double x2 = ( xmax - xmin ) / 2d;
    double y2 = ( ymax - ymin ) / 2d;

    // create the four quarters (tiles) for the submitted image and call this
    // method
    // in a recursion to create the next resolution level
    for( int i = 0; i < 4; i++ )
    {
      switch( i )
      {
      case 0:
      {
        // tile bounding box
        xmin_ = xmin;
        ymin_ = ymin;
        xmax_ = xmin + x2;
        ymax_ = ymin + y2;
        im = img.getSubimage( 0, img.getHeight() / 2, img.getWidth() / 2, img.getHeight() / 2 );
        break;
      }
      case 1:
      {
        // tile bounding box
        xmin_ = xmin + x2;
        ymin_ = ymin;
        xmax_ = xmax;
        ymax_ = ymin + y2;
        im = img.getSubimage( img.getWidth() / 2, img.getHeight() / 2, img.getWidth() / 2, img
            .getHeight() / 2 );
        break;
      }
      case 2:
      {
        // tile bounding box
        xmin_ = xmin;
        ymin_ = ymin + y2;
        xmax_ = xmin + x2;
        ymax_ = ymax;
        im = img.getSubimage( 0, 0, img.getWidth() / 2, img.getHeight() / 2 );
        break;
      }
      case 3:
      {
        // tile bounding box
        xmin_ = xmin + x2;
        ymin_ = ymin + y2;
        xmax_ = xmax;
        ymax_ = ymax;
        im = img.getSubimage( img.getWidth() / 2, 0, img.getWidth() / 2, img.getHeight() / 2 );
        break;
      }
      }

      // calculate the tiles width and height for the current resolution
      int tilex = (int)Math.round( ( xmax_ - xmin_ ) / targetResolutions[res] );
      int tiley = (int)Math.round( ( ymax_ - ymin_ ) / targetResolutions[res] );
      dir.setAttribute( "tileWidth", "" + tilex );
      dir.setAttribute( "tileHeight", "" + tiley );

      //BufferedImage tmp = new BufferedImage( tilex, tiley,
      // BufferedImage.TYPE_INT_RGB );
      // create new image (tile) with the same color model as the source image
      BufferedImage tmp = img.getSubimage( 0, 0, tilex, tiley );
      tmp = new BufferedImage( img.getColorModel(), tmp.getRaster()
          .createCompatibleWritableRaster(), true, new Hashtable() );

      Graphics g = tmp.getGraphics();
      g.drawImage( im, 0, 0, tilex, tiley, null );
      g.dispose();

      // observer output
      progressObserver.write( new Integer( count ) );

      // save tile to the filesystem
      saveTile( targetDir + "/l" + targetResolutions[res], xmin_, ymin_, tmp );

      createWorldFile( targetDir + "/l" + targetResolutions[res], xmin_, ymin_, xmax_, ymax_,
          tilex, tiley );

      storeEnvelope( targetDir + "/l" + targetResolutions[res], targetResolutions[res], xmin_,
          ymin_, xmax_, ymax_ );
      // recursion !
      tile( im, xmin_, ymin_, xmax_, ymax_, res + 1, dir );
    }
  }

  private void storeEnvelope( String dir, double res, double xmin, double ymin, double xmax,
      double ymax )
  {
    FeatureTypeProperty[] ftp = new FeatureTypeProperty[3];
    ftp[0] = FeatureFactory.createFeatureTypeProperty( "GEOM",
        "org.deegree.model.geometry.GM_Object", false );
    ftp[1] = FeatureFactory.createFeatureTypeProperty( "FILENAME", "java.lang.String", false );
    ftp[2] = FeatureFactory.createFeatureTypeProperty( "FOLDER", "java.lang.String", false );
    FeatureType ftype = FeatureFactory.createFeatureType( null, null, "tiles", ftp );

    try
    {
      GM_Envelope env = GeometryFactory.createGM_Envelope( xmin, ymin, xmax, ymax );
      GM_Object geom = GeometryFactory.createGM_Surface( env, null );
      FeatureProperty[] props = new FeatureProperty[3];
      props[0] = FeatureFactory.createFeatureProperty( "GEOM", geom );
      DecimalFormat fo = new DecimalFormat( "#.0" );
      String sx = fo.format( xmin * 1000 );
      if( xmin == 0 )
      {
        sx = "0000.0";
      }
      sx = sx.substring( 0, sx.length() - 3 ) + "0";

      String sy = fo.format( ymin * 1000 );
      if( ymin == 0 )
      {
        sy = "0000.0";
      }
      sy = sy.substring( 0, sy.length() - 3 ) + "0";

      String file = sx + "_" + sy + targetFormat;
      props[1] = FeatureFactory.createFeatureProperty( "FILENAME", file );
      props[2] = FeatureFactory.createFeatureProperty( "FOLDER", dir );
      Feature feat = FeatureFactory.createFeature( "file", ftype, props );
      FeatureCollection fc = (FeatureCollection)featColl.get( "fc" + res );
      fc.appendFeature( feat );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

  }

  private void createWorldFile( String dir, double xmin_, double ymin_, double xmax_, double ymax_,
      double tilex, double tiley ) throws Exception
  {
    DecimalFormat fo = new DecimalFormat( "#.0" );
    String sx = fo.format( xmin_ * 1000 );
    if( xmin_ == 0 )
    {
      sx = "0000.0";
    }
    sx = sx.substring( 0, sx.length() - 3 ) + "0";

    String sy = fo.format( ymin_ * 1000 );
    if( ymin_ == 0 )
    {
      sy = "0000.0";
    }
    sy = sy.substring( 0, sy.length() - 3 ) + "0";

    String file = dir + "/" + sx + "_" + sy + ".wld";
    FileWriter fos = new FileWriter( file );
    double resx = ( xmax_ - xmin_ ) / (double)tilex;
    double resy = ( ymin_ - ymax_ ) / (double)tiley;
    fos.write( resx + "\n" );
    fos.write( "0\n" );
    fos.write( "0\n" );
    fos.write( resy + "\n" );
    fos.write( ( xmin_ + resx / 2d ) + "\n" );
    fos.write( ( ymax_ - resy / 2d ) + "\n" );
    fos.close();
  }

  /**
   * stores one image (tile) in the desired format to the desired target
   * directory.
   */
  private String saveTile( String dir, double x, double y, BufferedImage img ) throws Exception
  {
    DecimalFormat fo = new DecimalFormat( "#.0" );
    String sx = fo.format( x * 1000 );
    if( x == 0 )
    {
      sx = "0000.0";
    }
    sx = sx.substring( 0, sx.length() - 3 ) + "0";

    String sy = fo.format( y * 1000 );
    if( y == 0 )
    {
      sy = "0000.0";
    }
    sy = sy.substring( 0, sy.length() - 3 ) + "0";
    count++;

    //String file = dir + "/tile_" + (startIndex++) + "." + targetFormat;
    String file = dir + "/" + sx + "_" + sy + "." + targetFormat;
    FileOutputStream fos = new FileOutputStream( file );

    if( targetFormat.equalsIgnoreCase( "bmp" ) )
    {
      Encoders.encodeBmp( fos, img );
    }
    else if( targetFormat.equalsIgnoreCase( "gif" ) )
    {
      Encoders.encodeGif( fos, img );
    }
    else if( targetFormat.equalsIgnoreCase( "png" ) )
    {
      Encoders.encodePng( fos, img );
    }
    else if( targetFormat.equalsIgnoreCase( "tiff" ) || targetFormat.equalsIgnoreCase( "tif" ) )
    {
      Encoders.encodeTiff( fos, img );
    }
    else if( targetFormat.equalsIgnoreCase( "jpg" ) || targetFormat.equalsIgnoreCase( "jpeg" ) )
    {
      Encoders.encodeJpeg( fos, img, quality );
    }

    fos.close();
    return file;
  }

  /**
   * adds a new Directory-node to the GVDescriptor XML-document
   */
  private Element addDirectory( int tileWidth, int tileHeight, String dirName, double widthCRS,
      double heightCRS, Element parent ) throws Exception
  {
    // create directory node
    Element dir = parent.getOwnerDocument().createElement( "Directory" );
    dir.setAttribute( "extensions", targetFormat );
    dir.setAttribute( "tileWidth", "" + tileWidth );
    dir.setAttribute( "tileHeight", "" + tileHeight );
    dir.setAttribute( "minx", "" + xmin );
    dir.setAttribute( "miny", "" + ymin );
    dir.setAttribute( "maxx", "" + xmax );
    dir.setAttribute( "maxy", "" + ymax );
    dir.setAttribute( "widthCRS", "" + widthCRS );
    dir.setAttribute( "heightCRS", "" + heightCRS );

    Text text = parent.getOwnerDocument().createTextNode( dirName );
    dir.appendChild( text );

    parent.appendChild( dir );
    return dir;
  }

  /**
   * Gets the latitude and longitude coordinates (xmin, ymin, xmax and ymax) of
   * the image.
   */
  private void readWorldFile( String filename ) throws Exception
  {
    try
    {
      // Gets the substring beginning at the specified beginIndex (0) - the
      // beginning index, inclusive - and extends to the character at
      //	index endIndex (position of '.') - the ending index, exclusive.
      String fname = null;
      int pos = filename.lastIndexOf( "." );
      filename = filename.substring( 0, pos );

      //Looks for corresponding worldfiles.
      if( ( new File( filename + ".tfw" ) ).exists() )
      {
        fname = filename + ".tfw";
      }
      else if( ( new File( filename + ".wld" ) ).exists() )
      {
        fname = filename + ".wld";
      }
      else if( ( new File( filename + ".jgw" ) ).exists() )
      {
        fname = filename + ".jgw";
      }
      else if( ( new File( filename + ".jpgw" ) ).exists() )
      {
        fname = filename + ".jpgw";
      }
      else if( ( new File( filename + ".gfw" ) ).exists() )
      {
        fname = filename + ".gfw";
      }
      else if( ( new File( filename + ".gifw" ) ).exists() )
      {
        fname = filename + ".gifw";
      }
      else if( ( new File( filename + ".pgw" ) ).exists() )
      {
        fname = filename + ".pgw";
      }
      else if( ( new File( filename + ".pngw" ) ).exists() )
      {
        fname = filename + ".pngw";
      }
      else
      {
        throw new Exception( "Not a world file for: " + filename );
      }

      // Reads character files.
      // The constructors of this class (FileReader) assume that the default
      // character
      //	 encoding and the default byte-buffer size are appropriate.
      // The BufferedReader reads text from a character-input stream, buffering
      // characters so as
      //	to provide for the efficient reading of characters
      BufferedReader br = new BufferedReader( new FileReader( fname ) );

      String s = null;

      int cnt = 0;
      double d1 = 0;
      double d2 = 0;
      double d3 = 0;
      double d4 = 0;

      while( ( s = br.readLine() ) != null )
      {
        cnt++;
        s = s.trim();

        switch( cnt )
        {
        case 1:
          d1 = Double.parseDouble( s );
          break;
        case 4:
          d2 = Double.parseDouble( s );
          break;
        case 5:
          d3 = Double.parseDouble( s );
          break;
        case 6:
          d4 = Double.parseDouble( s );
          break;
        }
      }

      br.close();

      d3 = d3 - d1 / 2d;
      d4 = d4 + d2 / 2d;
      double d5 = d3 + ( image.getWidth() * d1 );
      double d6 = d4 + ( image.getHeight() * d2 );

      ymax = d4;
      ymin = d6;
      xmax = d5;
      xmin = d3;

      resx = ( xmax - xmin ) / image.getWidth();
      resy = ( ymax - ymin ) / image.getHeight();
    }
    catch( Exception ex )
    {
      ex.printStackTrace();
    }
  }

  /**
   * description: Extracts the GeoKeys of the GeoTIFF. The Following Tags will
   * be extracted(http://www.remotesensing.org/geotiff/spec/geotiffhome.html):
   * ModelPixelScaleTag = 33550 (SoftDesk) ModelTiepointTag = 33922 (Intergraph)
   * implementation status: working
   */
  private void readBBoxFromGeoTIFF( RenderedOp rop ) throws Exception
  {

    TIFFDirectory tifDir = (TIFFDirectory)rop.getDynamicProperty( "tiff_directory" );

    /*
     * for (int i=0; i < tifDir.getFields().length; i++) { System.out.println("
     * tiff-tag: " + tifDir.getFields()[i].getTag() + " type: " +
     * tifDir.getFields()[i].getType()); }
     */

    TIFFField modelPixelScaleTag = tifDir.getField( 33550 );
    resx = modelPixelScaleTag.getAsDouble( 0 );
    resy = modelPixelScaleTag.getAsDouble( 1 );

    TIFFField modelTiepointTag = tifDir.getField( 33922 );
    double val1 = 0.0;
    val1 = modelTiepointTag.getAsDouble( 0 );
    double val2 = 0.0;
    val2 = modelTiepointTag.getAsDouble( 1 );
    double val4 = 0.0;
    val4 = modelTiepointTag.getAsDouble( 3 );
    double val5 = 0.0;
    val5 = modelTiepointTag.getAsDouble( 4 );

    if( ( resx == 0.0 || resy == 0.0 )
        || ( val1 == 0.0 && val2 == 0.0 && val4 == 0.0 && val5 == 0.0 ) )
    {
      throw new Exception( "The image/coverage hasn't a bounding box" );
      //set the geoparams derived by geoTiffTags
    }
    else
    {

      // upper/left pixel
      double xOrigin = val4 - ( val1 * resx );
      double yOrigin = val5 - ( val2 * resy );

      // lower/right pixel
      double xRight = xOrigin + rop.getWidth() * resx;
      double yBottom = yOrigin - rop.getHeight() * resy;
      System.out.println( "resx: " + resx );
      System.out.println( "resy: " + resy );
      xmin = xOrigin;
      ymin = yBottom;
      xmax = xRight;
      ymax = yOrigin;

    }
    System.out.println( xmin + " " + ymin + " " + xmax + " " + ymax );
  }

  /**
   * adds a new grid coverage layer to a WCS
   */
  private void addGridCoverageLayer( Node node, String layerID, String title, String crs,
      double llminx, double llminy, double llmaxx, double llmaxy, double minx, double miny,
      double maxx, double maxy, double width, int height ) throws Exception
  {
    StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<wcs:GridCoverageLayer xmlns:wcs=\"http://www.opengis.net/wcs\">" );
    sb.append( "<wcs:LayerID>" + layerID + "</wcs:LayerID>" );
    sb.append( "<wcs:Title>" + title + "</wcs:Title>" );
    sb.append( "<wcs:LatLonBoundingBox minx=\"" + llminx + "\" " );
    sb.append( "miny=\"" + llminy + "\" maxx=\"" + llmaxx + "\" maxy=\"" + llmaxy + "\"/>" );
    sb.append( "<wcs:SRS>" + crs + "</wcs:SRS>" );
    sb.append( "<wcs:GridExtentDescription>" );
    sb.append( "<wcs:SpatialExtent srsName=\"" + crs + "\">" );
    sb.append( "<wcs:XExtent>" );
    sb.append( "<wcs:min>" + minx + "</wcs:min>" );
    sb.append( "<wcs:max>" + maxx + "</wcs:max>" );
    sb.append( "</wcs:XExtent><wcs:YExtent>" );
    sb.append( "<wcs:min>" + miny + "</wcs:min>" );
    sb.append( "<wcs:max>" + maxy + "</wcs:max>" );
    sb.append( "</wcs:YExtent></wcs:SpatialExtent>" );
    sb.append( "<wcs:GridAxisDescription><wcs:GridAxis>" );
    sb.append( "<wcs:Name>X</wcs:Name>" );
    sb.append( "<wcs:orientation>right</wcs:orientation>" );
    sb.append( "</wcs:GridAxis><wcs:GridAxis>" );
    sb.append( "<wcs:Name>Y</wcs:Name>" );
    sb.append( "<wcs:orientation>up</wcs:orientation>" );
    sb.append( "</wcs:GridAxis></wcs:GridAxisDescription>" );
    sb.append( "<wcs:Grid dimension=\"2\" type=\"post\">" );
    sb.append( "<wcs:GridRange><wcs:low>" );
    sb.append( "<wcs:ordinate>0</wcs:ordinate>" );
    sb.append( "<wcs:ordinate>0</wcs:ordinate>" );
    sb.append( "</wcs:low><wcs:high>" );
    sb.append( "<wcs:ordinate>" + width + "</wcs:ordinate>" );
    sb.append( "<wcs:ordinate>" + height + "</wcs:ordinate>" );
    sb.append( "</wcs:high></wcs:GridRange></wcs:Grid>" );
    sb.append( "</wcs:GridExtentDescription>" );
    sb.append( "<wcs:RangeSetDescription>" );
    sb.append( "<wcs:GridRangeDescription>" );
    sb.append( "<wcs:RangeID>" + layerID + "</wcs:RangeID>" );
    sb.append( "<wcs:title>" + title + "</wcs:title>" );
    sb.append( "</wcs:GridRangeDescription>" );
    sb.append( "</wcs:RangeSetDescription></wcs:GridCoverageLayer>" );

    // parse new GridCoverage Layer as dom document and get the root element
    StringReader sr = new StringReader( sb.toString() );
    Document doc = XMLTools.parse( sr );
    Node root = doc.getDocumentElement();

    // insert the new GridCoverage layer into the existing CoverageLayerList
    XMLTools.insertNodeInto( root, node );
  }

  /**
   *  
   */
  private static void printHelp()
  {
    System.out.println( "ERROR: List of submitted parameters isn't complete." );
    System.out.println();
    System.out.println( "TileImageTree parameters: " );
    System.out.println( "-i: input directory containing the image(s) " );
    System.out.println( "    to be tiled (mandatory)" );
    System.out.println( "-o: output directory path name (mandatory)" );
    System.out.print( "-f: output format (gif, bmp, jpg, png, tif)" );
    System.out.println( " default = jpg; " );
    System.out.println( "    Consider that the target format must have the" );
    System.out.println( "    same or a higher color depht then the input format" );
    System.out.println( "-r comma sperated list of resolutions; e.g. 1.0,0.5,0.25" );
    System.out.println( "    The length of the list is equal to the number of levels" );
    System.out.println( "    that will be generated. The number of level determines the" );
    System.out.println( "    size of the generated tiles because for each level the tiles" );
    System.out.println( "    of the former level will be devided into four quarters." );
    System.out.println( "    The first level will have the first resolution, the second." );
    System.out.println( "    level the second one etc.." );
    System.out.println( "-s: index where the nameing of the tiles start" );
    System.out.println( "     (optional, default = 0)" );
    System.out.println( "-q: qualitiy of the produced tiles (just if output format = jpg)" );
    System.out.println( "     (optional, default = 1 (best))" );
    System.out.println( "-k: coordinate reference system of the map to be tiled" );
    System.out.println( "     (optional, default = EPSG:4326)" );
  }

  /**
   * 
   * 
   * @param map
   * 
   * @return
   */
  private static boolean validate( HashMap map )
  {
    boolean valid = true;

    if( ( map.get( "-i" ) == null ) || ( map.get( "-o" ) == null ) || ( map.get( "-r" ) == null )
        || ( map.get( "-f" ) == null ) || ( map.get( "-h" ) != null ) )
    {
      valid = false;
    }

    return valid;
  }

  /**
   * @param args
   *          the command line arguments
   */
  public static void main( String[] args )
  {
    HashMap map = new HashMap();

    for( int i = 0; i < args.length; i += 2 )
    {
      map.put( args[i], args[i + 1] );
    }

    if( !validate( map ) )
    {
      printHelp();
      System.exit( 1 );
    }

    String inDir = ( (String)map.get( "-i" ) ).trim();

    if( !inDir.endsWith( "/" ) )
    {
      inDir = inDir + "/";
    }

    String outDir = ( (String)map.get( "-o" ) ).trim();
    if( !outDir.endsWith( "/" ) )
    {
      outDir = outDir + "/";
    }

    String format = ( (String)map.get( "-f" ) ).toUpperCase();

    double[] targetResolutions = null;

    try
    {
      StringTokenizer st = new StringTokenizer( (String)map.get( "-r" ), ",;" );
      targetResolutions = new double[st.countTokens()];

      for( int i = 0; i < targetResolutions.length; i++ )
      {
        double v = Double.parseDouble( st.nextToken() );
        targetResolutions[i] = v;
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
      System.out.println( "Cant't parse target resolutions!" + e );
      printHelp();
      System.exit( 1 );
    }

    int startIndex = 0;

    try
    {
      startIndex = Integer.parseInt( (String)map.get( "-s" ) );
    }
    catch( Exception ex )
    {}

    float quality = 1.0f;

    try
    {
      quality = Float.parseFloat( (String)map.get( "-q" ) );

      if( quality > 1 )
      {
        quality = 1.0f;
      }
      else if( quality < 0.1 )
      {
        quality = 0.1f;
      }
    }
    catch( Exception ex )
    {}

    String crs = (String)map.get( "-k" );

    if( crs == null )
    {
      crs = "EPSG:4326";
    }

    try
    {
      File file = new File( inDir );
      String[] list = file.list( new DFileFilter() );

      for( int i = 0; i < list.length; i++ )
      {
        int pos = list[i].lastIndexOf( '.' );
        String oDir = outDir + list[i].substring( 0, pos );
        System.out.println( outDir );

        //File file_ = new File( outDir );
        //if ( !file_.exists() ) {
        String inFile = inDir + list[i];
        TileImageTree2 tit = new TileImageTree2( inFile, oDir, format, targetResolutions,
            startIndex, quality, crs );
        tit.createTileImageTree();
        //}
      }

      new UniteGvDesc( outDir, "allGvDesc.xml", crs );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  //                        inner classes //
  ///////////////////////////////////////////////////////////////////////////

  /**
   * default progress observer class. write the progress in % to the console
   */
  public class ProgressObserver
  {
    private int max = 0;

    /**
     * Creates a new ProgressObserver object.
     */
    public ProgressObserver()
    {
      for( int i = 0; i < targetResolutions.length; i++ )
      {
        max += (int)Math.pow( 4, ( i + 1 ) );
      }
    }

    /**
     * 
     * 
     * @param object
     */
    public void write( Object object )
    {
      double v = ( (Integer)object ).intValue();

      if( ( v % 30 ) == 0 )
      {
        //System.gc();
        v = (int)Math.round( v / max * 10000d );
        System.out.println( ( v / 100d ) + "%" );
      }
    }
  }

  /**
   * 
   * 
   * @version $Revision$
   * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
   */
  private static class DFileFilter implements FilenameFilter
  {
    /**
     * @return
     */
    public boolean accept( File f, String name )
    {
      int pos = name.lastIndexOf( "." );
      String ext = name.substring( pos + 1 );
      return ext.toUpperCase().equals( "JPG" ) || ext.toUpperCase().equals( "TIFF" )
          || ext.toUpperCase().equals( "TIF" ) || ext.toUpperCase().equals( "GIF" )
          || ext.toUpperCase().equals( "PNG" ) || ext.toUpperCase().equals( "BMP" )
          || ext.toUpperCase().equals( "JPEG" );
    }
  }

  /**
   * 
   * 
   * @version $Revision$
   * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
   */
  private static class UniteGvDesc
  {
    /** Creates a new instance of UniteGvDesc */
    public UniteGvDesc( String rootDir, String targetFile, String crs )
    {
      if( !rootDir.endsWith( "/" ) )
      {
        rootDir = rootDir + "/";
      }

      try
      {
        Reader reader = new FileReader( rootDir + "gvDesc.xml" );
        Document mainDoc = XMLTools.parse( reader );
        Element mainLevel = XMLTools.getNamedChild( mainDoc.getDocumentElement(), "Level" );

        DDFileFilter fileFilter = new DDFileFilter();
        File file = new File( rootDir );
        String[] dirs = file.list( fileFilter );

        for( int i = 0; i < dirs.length; i++ )
        {
          File ff = new File( rootDir + dirs[i] );

          if( ff.isDirectory() )
          {
            Document doc = XMLTools.parse( rootDir + dirs[i] + "/gvDesc.xml" );
            Element elem = doc.getDocumentElement();
            Element el = XMLTools.getNamedChild( elem, "Level" );
            el = XMLTools.getNamedChild( el, "Directory" );
            XMLTools.insertNodeInto( el, mainLevel );
          }
        }

        FileOutputStream fos = new FileOutputStream( rootDir + targetFile );
        String s = DOMPrinter.nodeToString( mainDoc, "UTF-8" );
        s = StringExtend.replace( s, "EPSG:4326", crs, true );
        fos.write( s.getBytes( "UTF-8" ) );
        fos.close();
        System.out.println( "Don't forget to set the correct bounding box and " );
        System.out.println( "raster size (pixel) in the allGvDesc.xml. Because this" );
        System.out.println( "have not been done automaticly!" );
        Thread.sleep( 10000 );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }

    /**
     *  
     */
    private class DDFileFilter implements FilenameFilter
    {
      /**
       * 
       * 
       * @return
       */
      public String getDescription()
      {
        return "*.XML";
      }

      /**
       * 
       * @return
       */
      public boolean accept( File f, String name )
      {
        int pos = name.lastIndexOf( "." );
        String ext = name.substring( pos + 1 );
        return ( ext.toUpperCase().equals( "XML" ) || f.isDirectory() );
      }
    }
  }
}