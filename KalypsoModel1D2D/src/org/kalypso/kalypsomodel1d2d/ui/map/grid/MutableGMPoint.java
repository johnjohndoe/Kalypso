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
  private GM_Point point;

  public MutableGMPoint( final GM_Point point )
  {
    Assert.throwIAEOnNull( point, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.MutableGMPoint.0" ) ); //$NON-NLS-1$
    this.point = point;
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Point#getAsArray()
   */
  @Override
  public double[] getAsArray( )
  {
    return point.getAsArray();
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Point#getPosition()
   */
  @Override
  public GM_Position getPosition( )
  {
    return point.getPosition();
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Point#getX()
   */
  @Override
  public double getX( )
  {
    return point.getX();
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Point#getY()
   */
  @Override
  public double getY( )
  {
    return point.getY();
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Point#getZ()
   */
  @Override
  public double getZ( )
  {
    return point.getZ();
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#contains(org.kalypsodeegree.model.geometry.GM_Object)
   */
  @Override
  public boolean contains( final GM_Object gmo )
  {
    return point.contains( gmo );
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#contains(org.kalypsodeegree.model.geometry.GM_Position)
   */
  @Override
  public boolean contains( final GM_Position position )
  {
    return point.contains( position );
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#difference(org.kalypsodeegree.model.geometry.GM_Object)
   */
  @Override
  public GM_Object difference( final GM_Object gmo )
  {
    return gmo.difference( gmo );
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#distance(org.kalypsodeegree.model.geometry.GM_Object)
   */
  @Override
  public double distance( final GM_Object gmo )
  {
    return point.distance( gmo );
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#getBoundary()
   */
  @Override
  public GM_Boundary getBoundary( )
  {
    return point.getBoundary();
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#getBuffer(double)
   */
  @Override
  public GM_Object getBuffer( final double distance )
  {
    return point.getBuffer( distance );
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#getCentroid()
   */
  @Override
  public GM_Point getCentroid( )
  {
    return point.getCentroid();
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#getConvexHull()
   */
  @Override
  public GM_Object getConvexHull( ) throws GM_Exception
  {
    return point.getConvexHull();
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#getCoordinateDimension()
   */
  @Override
  public int getCoordinateDimension( )
  {
    return point.getCoordinateDimension();
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#getCoordinateSystem()
   */
  @Override
  public String getCoordinateSystem( )
  {
    return point.getCoordinateSystem();
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#getDimension()
   */
  @Override
  public int getDimension( )
  {
    return point.getDimension();
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#getEnvelope()
   */
  @Override
  public GM_Envelope getEnvelope( )
  {
    return point.getEnvelope();
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#intersection(org.kalypsodeegree.model.geometry.GM_Object)
   */
  @Override
  public GM_Object intersection( final GM_Object gmo )
  {
    return point.intersection( gmo );
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#intersects(org.kalypsodeegree.model.geometry.GM_Object)
   */
  @Override
  public boolean intersects( final GM_Object gmo )
  {
    return point.intersects( gmo );
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#invalidate()
   */
  @Override
  public void invalidate( )
  {
    point.invalidate();
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#isEmpty()
   */
  @Override
  public boolean isEmpty( )
  {
    return point.isEmpty();
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#isWithinDistance(org.kalypsodeegree.model.geometry.GM_Object,
   *      double)
   */
  @Override
  public boolean isWithinDistance( final GM_Object gmo, final double distance )
  {
    return point.isWithinDistance( gmo, distance );
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#setCoordinateSystem(java.lang.String)
   */
  @Override
  public void setCoordinateSystem( final String crs )
  {
    point.setCoordinateSystem( crs );
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#translate(double[])
   */
  @Override
  public void translate( final double[] d )
  {
    point.translate( d );
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#union(org.kalypsodeegree.model.geometry.GM_Object)
   */
  @Override
  public GM_Object union( final GM_Object gmo )
  {
    return point.union( gmo );
  }

  public void setPoint( final GM_Point point )
  {
    this.point = point;
  }

  /**
   * @see java.lang.Object#clone()
   */
  @Override
  public Object clone( ) throws CloneNotSupportedException
  {
    throw new CloneNotSupportedException();
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  @Override
  public Object getAdapter( final Class adapter )
  {
    return null;
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#transform(java.lang.String)
   */
  @Override
  public GM_Object transform( String targetCRS ) throws Exception
  {
    return new MutableGMPoint( (GM_Point) this.point.transform( targetCRS ) );
  }
}