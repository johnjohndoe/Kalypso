/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.grid;

import java.io.IOException;
import java.net.URL;

import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;
import javax.media.jai.TiledImage;

import org.apache.commons.lang.NotImplementedException;
import org.geotiff.image.jai.GeoTIFFDirectory;
import org.kalypso.grid.GridFileVerifier.IMAGE_TYPE;
import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain.OffsetVector;
import org.kalypsodeegree_impl.model.cv.GridRange_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.libtiff.jai.codec.XTIFFField;

import com.sun.media.jai.codec.SeekableStream;

/**
 * {@link IGridMetaReader} implementation for Geo-Tiffs.
 * 
 * @author Dirk Kuch
 */
public class GridMetaReaderGeoTiff implements IGridMetaReader
{
  /**
   * GeoTiff differs between PixelIsArea and PixelIsRaster representation
   */
  private enum RASTER_TYPE
  {
    ePixelIsArea,
    ePixelIsPoint;

    public static RASTER_TYPE getRasterType( final GeoTIFFDirectory directory )
    {
      /**
       * http://remotesensing.org/geotiff/spec/geotiff6.html#6.3.1<br>
       * Values:<br>
       * RasterPixelIsArea = 1<br>
       * RasterPixelIsPoint = 2<br>
       */
      final XTIFFField pixelType = directory.getGeoKey( GTRasterTypeGeoKey );
      final int code = pixelType.getAsInt( 0 );

      if( code == 1 )
        return ePixelIsArea;
      else if( code == 2 )
        return ePixelIsPoint;

      // TODO default ?!?
      return ePixelIsArea;

    }
  }

  protected static int GTRasterTypeGeoKey = 1025;

  /**
   * tie points<br>
   * <br>
   * raster space <br>
   * m_tiepoints[0] = I<br>
   * m_tiepoints[1] = J<br>
   * m_tiepoints[2] = K<br>
   * <br>
   * model space<br>
   * m_tiepoints[3] = X<br>
   * m_tiepoints[4] = Y<br>
   * m_tiepoints[5] = Z<br>
   */
  private double[] m_tiepoints;

  private double[] m_pixelScales;

  private final URL m_urlImage;

  public GridMetaReaderGeoTiff( final URL urlImage, final IMAGE_TYPE type ) throws IOException
  {
    m_urlImage = urlImage;
    if( urlImage == null || type == null )
      throw new IllegalStateException( "invalid geotiff file" );

    if( !IMAGE_TYPE.eTIFF.equals( type ) )
      throw new IllegalStateException( urlImage.toString() + " isn't a geotiff file" );

    discover();
  }

  /**
   * discover geotiff settings - not completely implemented because lack of geotiff examples and tiff world file
   * representation
   */
  private void discover( ) throws IOException
  {
    final SeekableStream stream = SeekableStream.wrapInputStream( m_urlImage.openStream(), true );
    final GeoTIFFDirectory directory = new GeoTIFFDirectory( stream, 0 );

    /* determine pixel is area or point */
    final RASTER_TYPE rasterType = RASTER_TYPE.getRasterType( directory );
    if( RASTER_TYPE.ePixelIsPoint.equals( rasterType ) )
      throw new NotImplementedException();

    /* get tie points */
    m_tiepoints = directory.getTiepoints();

    /* get pixel scale */
    m_pixelScales = directory.getPixelScale();

    /* get transformationMatrix */
    final double[] transformationMatrix = directory.getTransformationMatrix();
    if( transformationMatrix != null )
      throw new NotImplementedException();

    stream.close();
  }

  /**
   * @see org.kalypso.gml.ui.wizard.imports.IRasterMetaReader#getCoverage(org.kalypsodeegree_impl.model.cv.RectifiedGridDomain.OffsetVector,
   *      org.kalypsodeegree_impl.model.cv.RectifiedGridDomain.OffsetVector, java.lang.Double[],
   *      org.opengis.cs.CS_CoordinateSystem)
   */
  public RectifiedGridDomain getCoverage( final OffsetVector offsetX, final OffsetVector offsetY, final Double[] upperLeftCorner, final String crs ) throws Exception
  {
    if( (offsetX == null) || (offsetY == null) || (upperLeftCorner == null) || (upperLeftCorner.length != 2) || (crs == null) )
      throw (new IllegalStateException());

    final RenderedOp image = JAI.create( "url", m_urlImage );
    final TiledImage tiledImage = new TiledImage( image, true );
    image.dispose();

    final int height = tiledImage.getHeight();
    final int width = tiledImage.getWidth();

    final double[] lows = new double[] { 0, 0 };
    final double[] highs = new double[] { width, height };

    final GridRange gridRange = new GridRange_Impl( lows, highs );
    final GM_Point origin = GeometryFactory.createGM_Point( upperLeftCorner[0], upperLeftCorner[1], crs );

    tiledImage.dispose();

    return new RectifiedGridDomain( origin, offsetX, offsetY, gridRange );
  }

  /**
   * @see org.kalypso.gml.ui.wizard.imports.IRasterMetaReader#getUpperLeftCornerX()
   */
  public String getOriginCornerX( )
  {
    return Double.toString( m_tiepoints[3] );
  }

  /**
   * @see org.kalypso.gml.ui.wizard.imports.IRasterMetaReader#getLowerLeftCornerY()
   */
  public String getOriginCornerY( )
  {
    return Double.toString( m_tiepoints[4] );
  }

  /**
   * @see org.kalypso.gml.ui.wizard.imports.IRasterMetaReader#getPixelDx()
   */
  public String getVectorXx( )
  {
    return Double.toString( m_pixelScales[0] );
  }

  /**
   * @see org.kalypso.gml.ui.wizard.imports.IRasterMetaReader#getPhiX()
   */
  public String getVectorXy( )
  {
    return Double.toString( 0 );
  }

  /**
   * @see org.kalypso.gml.ui.wizard.imports.IRasterMetaReader#getPhiY()
   */
  public String getVectorYx( )
  {
    return Double.toString( 0 );
  }

  /**
   * @see org.kalypso.gml.ui.wizard.imports.IRasterMetaReader#getPixelDy()
   */
  public String getVectorYy( )
  {
    return Double.toString( m_pixelScales[1] * -1.0 );
  }
}
