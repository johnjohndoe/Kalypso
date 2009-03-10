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
package org.kalypso.ogc.gml.widgets.tools;

import org.apache.commons.lang.NotImplementedException;

import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * @author Dirk Kuch
 */
public class AdvancedEditWidgetHelper
{
  private static double PRECISION = 0.00001;

  public enum DIRECTION
  {
    eForward,
    eBackward;
  }

  /**
   * @param direction
   *          sometime a ring contains the same point twice or more
   */
  public static int getIndexOfPoint( final LineString ring, final Point point, final DIRECTION direction )
  {
    if( DIRECTION.eForward.equals( direction ) )
    {
      for( int i = 0; i < ring.getNumPoints(); i++ )
      {
        final Point p = ring.getPointN( i );
        if( p.distance( point ) < PRECISION )
          return i;
      }

      return -1;
    }
    else if( DIRECTION.eBackward.equals( direction ) )
    {
      for( int i = ring.getNumPoints() - 1; i >= 0; i-- )
      {
        final Point p = ring.getPointN( i );
        if( p.distance( point ) < PRECISION )
          return i;
      }

      return -1;
    }
    else
      throw new NotImplementedException();
  }

  public static int resolveNeighbor( final LineString ring, final Point current, final int index, final DIRECTION direction )
  {
    if( DIRECTION.eForward.equals( direction ) )
    {
      if( index < ring.getNumPoints() - 1 )
      {
        final Point p = ring.getPointN( index + 1 );
        if( p.distance( current ) < PRECISION )
          return resolveNeighbor( ring, current, index + 1, direction );
        else
          return index + 1;

      }
      else
        return resolveNeighbor( ring, current, 0, direction );
    }
    else if( DIRECTION.eBackward.equals( direction ) )
    {
      if( index > 0 )
      {
        final Point p = ring.getPointN( index - 1 );
        if( p.distance( current ) < PRECISION )
          return resolveNeighbor( ring, current, index - 1, direction );
        else
          return index - 1;
      }
      else
        return resolveNeighbor( ring, current, ring.getNumPoints() - 1, direction );
    }

    throw new NotImplementedException();
  }
}
