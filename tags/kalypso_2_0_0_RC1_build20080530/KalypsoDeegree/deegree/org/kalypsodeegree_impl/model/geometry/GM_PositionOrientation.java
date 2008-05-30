/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.model.geometry;

import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * Helper class to set the orientation of a closed ring defined by its positions
 * 
 * @author Thomas Jung
 */
public class GM_PositionOrientation
{
  private static final Logger LOG = Logger.getLogger( GM_PositionOrientation.class.getName() );

  public static class TYPE
  {
    public static TYPE POSITIV = new TYPE( "positiv" );

    public static TYPE NEGATIV = new TYPE( "negativ" );

    private final String m_name;

    private TYPE( final String name )
    {
      m_name = name;
    }

    @Override
    public String toString( )
    {
      return m_name;
    }
  }

  private GM_Position[] m_positions;

  public static GM_Position[] orient( final GM_Position[] pos, final TYPE type ) throws GM_PositionOrientationException
  {
    if( getPolygonOrientation( pos ) != type )
      return reverse( pos );
    else
      return pos;
  }

  private static GM_Position[] reverse( final GM_Position[] pos )
  {
    final List<GM_Position> posList = new LinkedList<GM_Position>();

    for( int i = 0; i < pos.length; i++ )
    {
      posList.add( pos[pos.length - i - 1] );
    }

    return posList.toArray( new GM_Position[posList.size()] );
  }

  /**
   * Orientation of 2D-Polygon, taken from http://geometryalgorithms.com/Archive/algorithm_0101/algorithm_0101.htm
   * Original comment: orientation2D_Polygon(): tests the orientation of a simple polygon Input: int n = the number of
   * vertices in the polygon Point* V = an array of n+1 vertices with V[n]=V[0] Return: >0 for counterclockwise =0 for
   * none (degenerate) <0 for clockwise Note: this algorithm is faster than computing the signed area.
   * 
   * @throws CoordOrientationException
   */
  public static TYPE getPolygonOrientation( final GM_Position[] pos ) throws GM_PositionOrientationException
  {
    if( (pos == null) || (pos.length < 3) || !pos[0].equals( pos[pos.length - 1] ) )
      LOG.warning( "unable to filter positions: position do not form a ring" );

    // first find rightmost lowest vertex of the polygon
    int rmin = 0;
    double xmin = pos[0].getX();
    double ymin = pos[0].getY();

    for( int i = 1; i < pos.length - 1; i++ )
    {
      if( pos[i].getY() > ymin )
        continue;

      if( pos[i].getY() == ymin )
      {
        // just as low
        if( pos[i].getX() < xmin ) // and to left
          continue;
      }

      rmin = i; // a new rightmost lowest vertex
      xmin = pos[i].getX();
      ymin = pos[i].getY();
    }

    final GM_Position P0 = rmin == 0 ? pos[pos.length - 2] : pos[rmin - 1];
    final GM_Position P1 = pos[rmin];
    final GM_Position P2 = pos[rmin + 1];

    final double o = (P1.getX() - P0.getX()) * (P2.getY() - P0.getY()) - (P2.getX() - P0.getX()) * (P1.getY() - P0.getY());

    if( o < 0 )
      return TYPE.NEGATIV;
    else if( o > 0 )
      return TYPE.POSITIV;
    else
      throw new GM_PositionOrientationException( "Anwendungsfehler: degeneriertes Polygon aufgetreten" );
  }

  public GM_Position[] getPositions( )
  {
    return m_positions;
  }

  public void setPositions( final GM_Position[] positions )
  {
    m_positions = positions;
  }

}
