/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de


 history:

 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 

 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:

 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.graphics.displayelements;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.Raster;
import java.awt.image.SampleModel;
import java.awt.image.renderable.ParameterBlock;
import java.io.Serializable;
import java.util.Iterator;
import java.util.TreeMap;
import java.util.Vector;

import javax.media.jai.JAI;
import javax.media.jai.PlanarImage;
import javax.media.jai.RasterFactory;
import javax.media.jai.TiledImage;

import org.kalypsodeegree.graphics.displayelements.RasterDisplayElement;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;
import org.kalypsodeegree.graphics.sld.Interval;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.model.cv.RangeSet;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author N. Peiler
 *  
 */
public class RasterDisplayElement_Impl extends GeometryDisplayElement_Impl implements
    RasterDisplayElement, Serializable
{

  private TiledImage m_surrogateTiledImage;

  private boolean m_valid = false;

  private final int mode_intervalColorMapping = 0;

  private final int mode_valueColorMapping = 1;

  /**
   * Creates a new RasterDisplayElement_Impl object.
   * 
   * @param feature
   * @param geometry
   * @param symbolizer
   */
  protected RasterDisplayElement_Impl( Feature feature, GM_Object geometry,
      RasterSymbolizer symbolizer )
  {
    super( feature, geometry, symbolizer );
  }

  private TiledImage getImage()
  {
    if( m_surrogateTiledImage == null || !m_valid )
    {
      RasterSymbolizer rasterSym = (RasterSymbolizer)symbolizer;

      RectifiedGridDomain rgDomain = (RectifiedGridDomain)feature
          .getProperty( "rectifiedGridDomain" );
      RangeSet rangeSet = (RangeSet)feature.getProperty( "rangeSet" );

      Raster surrogateRaster = getSurrogateRaster( rgDomain, rangeSet, rasterSym );
      m_surrogateTiledImage = new TiledImage( getSurrogateImage( surrogateRaster ), true );
      m_valid = true;
    }
    return m_surrogateTiledImage;
  }

  /**
   * renders the DisplayElement to the submitted graphic context
   */
  public void paint( Graphics g, GeoTransform projection )
  {
    // cast Graphics to Graphics2D
    Graphics2D g2 = (Graphics2D)g;
    // get the geometry informations of the RectifiedGridCoverage
    RectifiedGridDomain rgDomain = (RectifiedGridDomain)feature.getProperty( "rectifiedGridDomain" );
    // create the target Coordinate system
    // TODO get target Coordinate system from projection
    String targetSrs = "EPSG:31469";
    ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
    CS_CoordinateSystem cs = org.kalypsodeegree_impl.model.cs.Adapters.getDefault().export(
        csFac.getCSByName( targetSrs ) );
    TiledImage rasterImage = getImage();

    try
    {
      drawRasterImage( g2, projection, rasterImage, rgDomain, cs );
    }
    catch( Exception e )
    {
      System.out.println( e );
    }
    /*
     * Graphics2D g2 = (Graphics2D)g; try {
     * 
     * PlanarImage image = getImage(); RectifiedGridDomain rgDomain =
     * (RectifiedGridDomain)feature .getProperty( "rectifiedGridDomain" );
     * String targetSrs = "EPSG:31469"; ConvenienceCSFactoryFull csFac = new
     * ConvenienceCSFactoryFull(); CS_CoordinateSystem cs =
     * org.kalypsodeegree_impl.model.cs.Adapters.getDefault().export(
     * csFac.getCSByName( targetSrs ) );
     * 
     * GM_Surface destSurface = rgDomain.getGM_Surface( cs ); GM_Ring
     * destExtRing = destSurface.getSurfaceBoundary().getExteriorRing();
     * GM_Position llCorner = destExtRing.getPositions()[0]; GM_Position
     * lrCorner = destExtRing.getPositions()[1]; GM_Position urCorner =
     * destExtRing.getPositions()[2]; GM_Position ulCorner =
     * destExtRing.getPositions()[3]; GM_Position pixel_llCorner =
     * projection.getDestPoint( llCorner ); GM_Position pixel_lrCorner =
     * projection.getDestPoint( lrCorner ); GM_Position pixel_urCorner =
     * projection.getDestPoint( urCorner ); GM_Position pixel_ulCorner =
     * projection.getDestPoint( ulCorner ); double destImageHeight =
     * pixel_llCorner.getY() - pixel_ulCorner.getY(); double destImageWidth =
     * pixel_lrCorner.getX() - pixel_llCorner.getX(); double scaleX =
     * destImageWidth / image.getWidth(); double scaleY = destImageHeight /
     * image.getHeight(); double shearX = pixel_llCorner.getX() -
     * pixel_ulCorner.getX(); double shearY = pixel_lrCorner.getY() -
     * pixel_llCorner.getY(); AffineTransform trafo = new AffineTransform();
     * trafo.scale( scaleX, scaleY ); trafo.translate( Math.abs( shearX ) /
     * Math.abs( scaleX ), Math.abs( shearY ) / Math.abs( scaleY ) );
     * trafo.shear( shearX / destImageHeight, shearY / destImageWidth );
     * 
     * GM_Position scaledImage_min = pixel_ulCorner; GM_Position scaledImage_max =
     * GeometryFactory.createGM_Position( pixel_urCorner.getX(),
     * pixel_llCorner.getY() );
     * 
     * GM_Position buffImage_min = GeometryFactory.createGM_Position(
     * scaledImage_min.getX() - Math.abs( shearX ), scaledImage_min.getY() -
     * Math.abs( shearY ) ); GM_Position buffImage_max =
     * GeometryFactory.createGM_Position( scaledImage_max.getX() + Math.abs(
     * shearX ), scaledImage_max.getY() + Math.abs( shearY ) ); GM_Envelope
     * buffImageEnv = GeometryFactory.createGM_Envelope( buffImage_min,
     * buffImage_max );
     * 
     * BufferedImage buffer = new BufferedImage( (int)buffImageEnv.getWidth(),
     * (int)buffImageEnv .getHeight(), BufferedImage.TYPE_INT_ARGB ); Graphics2D
     * bufferGraphics = (Graphics2D)buffer.getGraphics();
     * //bufferGraphics.setColor(Color.GREEN); bufferGraphics.setColor( new
     * Color( 255, 255, 255, 0 ) ); bufferGraphics.fillRect( 0, 0,
     * (int)buffImageEnv.getWidth(), (int)buffImageEnv.getHeight() );
     * bufferGraphics.drawRenderedImage( image, trafo ); g2.drawImage( buffer,
     * (int)buffImageEnv.getMin().getX(), (int)buffImageEnv.getMin().getY(),
     * null ); } catch( Exception e ) { // TODO Auto-generated catch block
     * e.printStackTrace(); }
     */
  }

  public void drawRasterImage( Graphics2D g2, GeoTransform projection, TiledImage rasterImage,
      RectifiedGridDomain gridDomain, CS_CoordinateSystem targetCS ) throws Exception
  {
    //  get the Screen extent in real coordinates
    GM_Envelope sourceScreenRect = projection.getSourceRect();
    // create a surface and transform it in the coordinate system of the
    // RectifiedGridCoverage
    GM_Surface sourceScreenSurface = GeometryFactory.createGM_Surface( sourceScreenRect, targetCS );
    GeoTransformer geoTrans1 = new GeoTransformer( gridDomain.getOrigin( null )
        .getCoordinateSystem() );
    GM_Surface destScreenSurface = (GM_Surface)geoTrans1.transform( sourceScreenSurface );
    // get the gridExtent for the envelope of the surface
    int[] gridExtent = gridDomain.getGridExtent( destScreenSurface.getEnvelope(), gridDomain
        .getOrigin( null ).getCoordinateSystem() );
    int lowX = gridExtent[0];
    int lowY = gridExtent[1];
    int highX = gridExtent[2];
    int highY = gridExtent[3];

    // calculate imageExtent from gridExtent
    int minX = lowX;
    int minY = rasterImage.getHeight() - highY;
    int width = highX - lowX;
    int height = highY - lowY;
    // get the required subImage according to the gridExtent
    PlanarImage image = rasterImage.getSubImage( minX, minY, width, height );

    // get the destinationSurface in target coordinates
    GM_Surface destSurface = gridDomain.getGM_Surface( lowX, lowY, highX, highY, targetCS );
    GM_Ring destExtRing = destSurface.getSurfaceBoundary().getExteriorRing();
    GM_Position llCorner = destExtRing.getPositions()[0];
    GM_Position lrCorner = destExtRing.getPositions()[1];
    GM_Position urCorner = destExtRing.getPositions()[2];
    GM_Position ulCorner = destExtRing.getPositions()[3];
    // calculate the Corners in screen coordinates
    GM_Position pixel_llCorner = projection.getDestPoint( llCorner );
    GM_Position pixel_lrCorner = projection.getDestPoint( lrCorner );
    GM_Position pixel_urCorner = projection.getDestPoint( urCorner );
    GM_Position pixel_ulCorner = projection.getDestPoint( ulCorner );
    // calculate the height and width of the image on screen
    double destImageHeight = pixel_llCorner.getY() - pixel_ulCorner.getY();
    double destImageWidth = pixel_lrCorner.getX() - pixel_llCorner.getX();
    // calculate the scaling factors for the transformation
    double scaleX = destImageWidth / image.getWidth();
    double scaleY = destImageHeight / image.getHeight();
    // calculate the shear parameters for the transformation
    double shearX = pixel_llCorner.getX() - pixel_ulCorner.getX();
    double shearY = pixel_lrCorner.getY() - pixel_llCorner.getY();

    GM_Surface orgDestSurface = gridDomain.getGM_Surface( targetCS );
    GM_Position orgULCorner = orgDestSurface.getSurfaceBoundary().getExteriorRing().getPositions()[3];
    GM_Position pixel_orgULCorner = projection.getDestPoint( orgULCorner );

    AffineTransform trafo = new AffineTransform();
    // translate the image, so that the subImage is at the right position
    trafo.translate( pixel_orgULCorner.getX() - pixel_ulCorner.getX(), pixel_orgULCorner.getY()
        - pixel_ulCorner.getY() );
    // scale the image
    trafo.scale( scaleX, scaleY );
    // translate the image to compensate the shearing
    trafo.translate( Math.abs( shearX ) / Math.abs( scaleX ), Math.abs( shearY )
        / Math.abs( scaleY ) );
    // shear the image
    trafo.shear( shearX / destImageHeight, shearY / destImageWidth );

    // calculate the required extent of the bufferedImage
    GM_Position scaledImage_min = pixel_ulCorner;
    GM_Position scaledImage_max = GeometryFactory.createGM_Position( pixel_urCorner.getX(),
        pixel_llCorner.getY() );

    GM_Position buffImage_min = GeometryFactory.createGM_Position( scaledImage_min.getX()
        - Math.abs( shearX ), scaledImage_min.getY() - Math.abs( shearY ) );
    GM_Position buffImage_max = GeometryFactory.createGM_Position( scaledImage_max.getX()
        + Math.abs( shearX ), scaledImage_max.getY() + Math.abs( shearY ) );
    GM_Envelope buffImageEnv = GeometryFactory.createGM_Envelope( buffImage_min, buffImage_max );

    BufferedImage buffer = new BufferedImage( (int)buffImageEnv.getWidth(), (int)buffImageEnv
        .getHeight(), BufferedImage.TYPE_INT_ARGB );
    Graphics2D bufferGraphics = (Graphics2D)buffer.getGraphics();
    //bufferGraphics.setColor(Color.GREEN);
    // draw a transparent backround on the bufferedImage
    bufferGraphics.setColor( new Color( 255, 255, 255, 0 ) );
    bufferGraphics.fillRect( 0, 0, (int)buffImageEnv.getWidth(), (int)buffImageEnv.getHeight() );
    // draw the image with the given transformation
    bufferGraphics.drawRenderedImage( image, trafo );
    // draw bufferedImage on the screen
    g2.drawImage( buffer, (int)buffImageEnv.getMin().getX(), (int)buffImageEnv.getMin().getY(),
        null );
  }

  /**
   * get a surrogate image for displaying with byte values of the given raster
   * with int values
   * 
   * @param surrogateRaster
   * @return surrogate image
   */
  private PlanarImage getSurrogateImage( Raster surrogateRaster )
  {
    PlanarImage surrogateImage = getPlanarImage( surrogateRaster );
    // convert the data type for displaying.
    ParameterBlock pbConvert = new ParameterBlock();
    pbConvert.addSource( surrogateImage );
    pbConvert.add( DataBuffer.TYPE_BYTE );
    surrogateImage = JAI.create( "format", pbConvert );
    return surrogateImage;
  }

  /**
   * get an image with the given Raster-Object
   * 
   * @param raster
   * @return Image
   */
  private PlanarImage getPlanarImage( Raster raster )
  {
    ColorModel colorModel = PlanarImage.createColorModel( raster.getSampleModel() );
    TiledImage tiledImage = new TiledImage( 0, 0, raster.getWidth(), raster.getHeight(), 0, 0,
        raster.getSampleModel(), colorModel );
    tiledImage.setData( raster );
    return tiledImage;
  }

  /**
   * creates a surrogate raster of the given rectifiedGridDomain and rangeSet
   * with the given colorTable
   * 
   * @param gridDomain
   * @return surrogate raster
   */
  private Raster getSurrogateRaster( RectifiedGridDomain gridDomain, RangeSet rangeSet,
      RasterSymbolizer rasterSym )
  {
    int mode = rasterSym.getMode();
    TreeMap intervalMap = null;
    if( mode == mode_intervalColorMapping )
    {
      intervalMap = rasterSym.getIntervalMap();
    }

    int nCols = gridDomain.getNumColumns();
    int nRows = gridDomain.getNumRows();
    SampleModel sampleModel = RasterFactory.createBandedSampleModel( DataBuffer.TYPE_INT, nCols,
        nRows, 4 );
    DataBuffer dataBuffer = sampleModel.createDataBuffer();
    Vector rangeSetData = rangeSet.getRangeSetData();
    for( int i = 0; i < rangeSetData.size(); i++ )
    {
      Vector rangeSetDataRow = (Vector)rangeSetData.get( i );
      for( int j = 0; j < rangeSetDataRow.size(); j++ )
      {
        Color actualColor = Color.DARK_GRAY;
        double actualOpacity = 1;
        if( rangeSetDataRow.get( j ) != null )
        {
          double actualValue = ( (Double)rangeSetDataRow.get( j ) ).doubleValue();
          switch( mode )
          {
          case mode_intervalColorMapping:
          {
            Iterator it = intervalMap.keySet().iterator();
            while( it.hasNext() )
            {
              Interval interval = (Interval)it.next();
              if( interval.contains( actualValue ) )
              {
                actualColor = (Color)intervalMap.get( interval );
                actualOpacity = actualColor.getAlpha();
                break;
              }
            }
            break;
          }
          case mode_valueColorMapping:
          {
            TreeMap colorMap = rasterSym.getColorMap();
            if( colorMap.containsKey( new Double( actualValue ) ) )
            {
              ColorMapEntry colorMapEntry = (ColorMapEntry)colorMap.get( new Double( actualValue ) );
              actualColor = colorMapEntry.getColor();
              actualOpacity = colorMapEntry.getOpacity();
            }
            break;
          }
          }
        }
        else
        {
          TreeMap colorMap = rasterSym.getColorMap();
          double nullValue = -9999;
          if( colorMap.containsKey( new Double( nullValue ) ) )
          {
            ColorMapEntry colorMapEntry = (ColorMapEntry)colorMap.get( new Double( nullValue ) );
            actualColor = colorMapEntry.getColor();
            actualOpacity = colorMapEntry.getOpacity();
          }
          else
          {
            actualColor = Color.WHITE;
            actualOpacity = 0;
          }
        }
        int redValue = actualColor.getRed();
        int greenValue = actualColor.getGreen();
        int blueValue = actualColor.getBlue();
        int alphaValue = (int)Math.round( actualOpacity * 255 );
        dataBuffer.setElem( 0, j + ( i * nCols ), redValue );
        dataBuffer.setElem( 1, j + ( i * nCols ), greenValue );
        dataBuffer.setElem( 2, j + ( i * nCols ), blueValue );
        dataBuffer.setElem( 3, j + ( i * nCols ), alphaValue );
      }
    }
    Point origin = new Point( 0, 0 );
    Raster surrogateRaster = RasterFactory.createWritableRaster( sampleModel, dataBuffer, origin );
    return surrogateRaster;
  }
}