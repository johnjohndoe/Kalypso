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
package org.kalypso.raster;

import java.io.File;
import java.io.IOException;

import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;
import javax.media.jai.TiledImage;

import org.kalypso.ogc.gml.WorldFile;
import org.kalypso.ogc.gml.WorldFileReader;
import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.cv.GridRange_Impl;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain.OffsetVector;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author kuch
 */
public class RasterMetaReaderWorldFile implements IRasterMetaReader
{

  private final File m_worldFile;

  private WorldFile m_world = null;

  private final File m_image;

  public RasterMetaReaderWorldFile( final File image, final File worldFile )
  {
    m_image = image;
    m_worldFile = worldFile;
    setup();
  }

  private void setup( )
  {
    if( (m_worldFile == null) || !(m_worldFile.exists()) )
    {
      throw (new IllegalStateException());
    }

    final WorldFileReader reader = new WorldFileReader();
    try
    {

      m_world = reader.readWorldFile( m_worldFile );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.gml.ui.wizard.imports.IRasterMetaReader#getLowerLeftCornerY()
   */
  public String getUpperLeftCornerY( )
  {
    return new Double( m_world.getUlcy() ).toString();
  }

  /**
   * @see org.kalypso.gml.ui.wizard.imports.IRasterMetaReader#getPhiX()
   */
  public String getVectorXy( )
  {
    return new Double( m_world.getRasterXGeoY() ).toString();
  }

  /**
   * @see org.kalypso.gml.ui.wizard.imports.IRasterMetaReader#getPhiY()
   */
  public String getVectorYx( )
  {
    return new Double( m_world.getRasterYGeoX() ).toString();
  }

  /**
   * @see org.kalypso.gml.ui.wizard.imports.IRasterMetaReader#getPixelDx()
   */
  public String getVectorXx( )
  {
    return new Double( m_world.getRasterXGeoX() ).toString();
  }

  /**
   * @see org.kalypso.gml.ui.wizard.imports.IRasterMetaReader#getPixelDy()
   */
  public String getVectorYy( )
  {
    return new Double( m_world.getRasterYGeoY() ).toString();
  }

  /**
   * @see org.kalypso.gml.ui.wizard.imports.IRasterMetaReader#getUpperLeftCornerX()
   */
  public String getUpperLeftCornerX( )
  {
    return new Double( m_world.getUlcx() ).toString();
  }

  /**
   * @see org.kalypso.gml.ui.wizard.imports.IRasterMetaReader#getCoverage(org.kalypsodeegree_impl.model.cv.RectifiedGridDomain.OffsetVector,
   *      org.kalypsodeegree_impl.model.cv.RectifiedGridDomain.OffsetVector, java.lang.Double[],
   *      org.opengis.cs.CS_CoordinateSystem)
   */
  public RectifiedGridDomain getCoverage( final OffsetVector offsetX, final OffsetVector offsetY, final Double[] upperLeftCorner, final CS_CoordinateSystem crs ) throws Exception
  {
    if( (offsetX == null) || (offsetY == null) || (upperLeftCorner == null) || (upperLeftCorner.length != 2) || (crs == null) )
    {
      throw (new IllegalStateException());
    }
    
    final RenderedOp image = JAI.create( "fileload", m_image.toString() );
    final TiledImage tiledImage = new TiledImage( image, true );
    final int height = tiledImage.getHeight();
    final int width = tiledImage.getWidth();

    final double[] lows = new double[] { 0, 0 };
    final double[] highs = new double[] { width, height };

    final GridRange gridRange = new GridRange_Impl( lows, highs );
    final GM_Point origin = GeometryFactory.createGM_Point( upperLeftCorner[0], upperLeftCorner[1], crs );

    return new RectifiedGridDomain( origin, offsetX, offsetY, gridRange );
  }

}
