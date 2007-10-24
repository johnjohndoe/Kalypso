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

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * A {@link IGeoGrid} implementation which delegates all method calls to a delegate grid.
 * <p>
 * The delegate can be changed during runtime, if it is not present a suitable exception is thrown.
 * </p>
 * 
 * @author Gernot Belger
 */
public abstract class AbstractDelegatingGeoGrid implements IGeoGrid
{
  private IGeoGrid m_delegate;

  public AbstractDelegatingGeoGrid( )
  {
    m_delegate = null;
  }

  public AbstractDelegatingGeoGrid( final IGeoGrid delegate )
  {
    m_delegate = delegate;
  }

  public void setDelegate( final IGeoGrid delegate )
  {
    m_delegate = delegate;
  }

  public IGeoGrid getDelegate( )
  {
    return m_delegate;
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#dispose()
   */
  public void dispose( )
  {
    // The delegate comes from outside, so it should be disposed outside
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getBoundingBox()
   */
  public Envelope getBoundingBox( ) throws GeoGridException
  {
    if( m_delegate == null )
      throw new GeoGridException( "No grid-delegate available", null );

    return m_delegate.getBoundingBox();
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getOffsetX()
   */
  public Coordinate getOffsetX( ) throws GeoGridException
  {
    if( m_delegate == null )
      throw new GeoGridException( "No grid-delegate available", null );

    return m_delegate.getOffsetX();
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getOffsetY()
   */
  public Coordinate getOffsetY( ) throws GeoGridException
  {
    if( m_delegate == null )
      throw new GeoGridException( "No grid-delegate available", null );

    return m_delegate.getOffsetY();
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getOrigin()
   */
  public Coordinate getOrigin( ) throws GeoGridException
  {
    if( m_delegate == null )
      throw new GeoGridException( "No grid-delegate available", null );

    return m_delegate.getOrigin();
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getSizeX()
   */
  public int getSizeX( ) throws GeoGridException
  {
    if( m_delegate == null )
      throw new GeoGridException( "No grid-delegate available", null );

    return m_delegate.getSizeX();
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getSizeY()
   */
  public int getSizeY( ) throws GeoGridException
  {
    if( m_delegate == null )
      throw new GeoGridException( "No grid-delegate available", null );

    return m_delegate.getSizeY();
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getValue(int, int)
   */
  public double getValue( final int x, final int y ) throws GeoGridException
  {
    if( m_delegate == null )
      throw new GeoGridException( "No grid-delegate available", null );

    return m_delegate.getValue( x, y );
  }

  /**
   * @see org.kalypso.grid.IGeoValueProvider#getValue(com.vividsolutions.jts.geom.Coordinate)
   */
  public double getValue( final Coordinate crd ) throws GeoGridException
  {
    if( m_delegate == null )
      throw new GeoGridException( "No grid-delegate available", null );

    return m_delegate.getValue( crd );
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getValueChecked(int, int)
   */
  public double getValueChecked( final int x, final int y ) throws GeoGridException
  {
    if( (x < 0) || (x >= getSizeX()) || (y < 0) || (y >= getSizeX()) )
      return Double.NaN;

    return getValue( x, y );
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getWalkingStrategy()
   */
  public IGeoWalkingStrategy getWalkingStrategy( ) throws GeoGridException
  {
    if( m_delegate == null )
      throw new GeoGridException( "No grid-delegate available", null );

    return m_delegate.getWalkingStrategy();
  }

}
