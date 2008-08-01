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

import java.io.File;
import java.net.URL;

import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain.OffsetVector;
import org.kalypsodeegree_impl.model.cv.GridRange_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * {@link IGridMetaReader} implementation for ESRI Ascii Grids.
 * 
 * @author Dirk Kuch
 */
public class GridMetaReaderAscii implements IGridMetaReader
{
  private final String m_cs;

  private RectifiedGridDomain m_domain;

  private final URL m_urlImage;

  private String m_noDataValue;

  public GridMetaReaderAscii( final URL urlImage, final String cs )
  {
    m_urlImage = urlImage;
    m_cs = cs;
    setup();
  }

  private void setup( )
  {
    if( m_urlImage == null )
      throw (new IllegalStateException());

    try
    {
      final AsciiGridReader reader = new AsciiGridReader( new File( m_urlImage.getFile() ) );
      m_domain = reader.getGridDomain( m_cs );
      m_noDataValue = reader.getNoDataValue();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.gml.ui.wizard.imports.IRasterMetaReader#getPhiX()
   */
  public String getVectorXy( )
  {
    return (new Double( m_domain.getOffsetX().getGeoY() ).toString());
  }

  /**
   * @see org.kalypso.gml.ui.wizard.imports.IRasterMetaReader#getPhiY()
   */
  public String getVectorYx( )
  {
    return (new Double( m_domain.getOffsetY().getGeoX() ).toString());
  }

  /**
   * @see org.kalypso.gml.ui.wizard.imports.IRasterMetaReader#getPixelDx()
   */
  public String getVectorXx( )
  {
    return (new Double( m_domain.getOffsetX().getGeoX() ).toString());
  }

  /**
   * @see org.kalypso.gml.ui.wizard.imports.IRasterMetaReader#getPixelDy()
   */
  public String getVectorYy( )
  {
    return (new Double( m_domain.getOffsetY().getGeoY() ).toString());
  }

  /**
   * @see org.kalypso.gml.ui.wizard.imports.IRasterMetaReader#getUpperLeftCornerX()
   */
  public String getOriginCornerX( )
  {
    try
    {
      /**
       * ASCII-Grid specification doesn't define an upper left corner, it has an origin point and we are returning this
       * point
       */
      return new Double( m_domain.getOrigin( m_cs ).getX() ).toString();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return null;
  }

  /**
   * @see org.kalypso.gml.ui.wizard.imports.IRasterMetaReader#getLowerLeftCornerY()
   */
  public String getOriginCornerY( )
  {
    try
    {
      /**
       * ASCII-Grid specification doesn't define an upper left corner, it has an origin point and we are returning this
       * point
       */
      return new Double( m_domain.getOrigin( m_cs ).getY() ).toString();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return null;
  }

  /**
   * @see org.kalypso.grid.IGridMetaReader#getCoverage(org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain.OffsetVector,
   *      org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain.OffsetVector, java.lang.Double[],
   *      java.lang.String)
   */
  public RectifiedGridDomain getCoverage( final OffsetVector offsetX, final OffsetVector offsetY, final Double[] upperLeftCorner, final String crs ) throws Exception
  {
    if( (offsetX == null) || (offsetY == null) || (upperLeftCorner == null) || (upperLeftCorner.length != 2) || (crs == null) )
      throw (new IllegalStateException());

    final double[] lows = new double[] { 0, 0 };
    final double[] highs = new double[] { m_domain.getNumColumns(), m_domain.getNumRows() };

    final GridRange gridRange = new GridRange_Impl( lows, highs );
    final GM_Point origin = GeometryFactory.createGM_Point( upperLeftCorner[0], upperLeftCorner[1], crs );

    return new RectifiedGridDomain( origin, offsetX, offsetY, gridRange );
  }

  public String getNoDataValue( )
  {
    return m_noDataValue;
  }
}
