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
package org.kalypso.kalypsomodel1d2d.ui.map.grid;

import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.transformation.transformer.GeoTransformerException;
import org.kalypsodeegree.model.geometry.GM_Boundary;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * Wrapped a {@link GM_Point} and mimes the possibility to changes it
 *
 * @author Patrice Congo
 */
class MutableGMPoint implements GM_Point
{
  private GM_Point m_point;

  public MutableGMPoint( final GM_Point point )
  {
    Assert.throwIAEOnNull( point, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.MutableGMPoint.0" ) ); //$NON-NLS-1$
    m_point = point;
  }

  @Override
  public double[] getAsArray( )
  {
    return m_point.getAsArray();
  }

  @Override
  public GM_Position getPosition( )
  {
    return m_point.getPosition();
  }

  @Override
  public double getX( )
  {
    return m_point.getX();
  }

  @Override
  public double getY( )
  {
    return m_point.getY();
  }

  @Override
  public double getZ( )
  {
    return m_point.getZ();
  }

  @Override
  public boolean contains( final GM_Object gmo )
  {
    return m_point.contains( gmo );
  }

  @Override
  public boolean contains( final GM_Position position )
  {
    return m_point.contains( position );
  }

  @Override
  public GM_Object difference( final GM_Object gmo )
  {
    return gmo.difference( gmo );
  }

  @Override
  public double distance( final GM_Object gmo )
  {
    return m_point.distance( gmo );
  }

  @Override
  public GM_Boundary getBoundary( )
  {
    return m_point.getBoundary();
  }

  @Override
  public GM_Object getBuffer( final double distance )
  {
    return m_point.getBuffer( distance );
  }

  @Override
  public GM_Point getCentroid( )
  {
    return m_point.getCentroid();
  }

  @Override
  public GM_Object getConvexHull( ) throws GM_Exception
  {
    return m_point.getConvexHull();
  }

  @Override
  public int getCoordinateDimension( )
  {
    return m_point.getCoordinateDimension();
  }

  @Override
  public String getCoordinateSystem( )
  {
    return m_point.getCoordinateSystem();
  }

  @Override
  public int getDimension( )
  {
    return m_point.getDimension();
  }

  @Override
  public GM_Envelope getEnvelope( )
  {
    return m_point.getEnvelope();
  }

  @Override
  public GM_Object intersection( final GM_Object gmo )
  {
    return m_point.intersection( gmo );
  }

  @Override
  public boolean intersects( final GM_Object gmo )
  {
    return m_point.intersects( gmo );
  }

  @Override
  public void invalidate( )
  {
    m_point.invalidate();
  }

  @Override
  public boolean isEmpty( )
  {
    return m_point.isEmpty();
  }

  @Override
  public boolean isWithinDistance( final GM_Object gmo, final double distance )
  {
    return m_point.isWithinDistance( gmo, distance );
  }

  @Override
  public void setCoordinateSystem( final String crs )
  {
    m_point.setCoordinateSystem( crs );
  }

  @Override
  public void translate( final double[] d )
  {
    m_point.translate( d );
  }

  @Override
  public GM_Object union( final GM_Object gmo )
  {
    return m_point.union( gmo );
  }

  public void setPoint( final GM_Point point )
  {
    m_point = point;
  }

  @Override
  public Object clone( ) throws CloneNotSupportedException
  {
    throw new CloneNotSupportedException();
  }

  @Override
  public Object getAdapter( final Class adapter )
  {
    return null;
  }

  @Override
  public GM_Object transform( final String targetCRS ) throws GeoTransformerException
  {
    return new MutableGMPoint( (GM_Point) m_point.transform( targetCRS ) );
  }
}