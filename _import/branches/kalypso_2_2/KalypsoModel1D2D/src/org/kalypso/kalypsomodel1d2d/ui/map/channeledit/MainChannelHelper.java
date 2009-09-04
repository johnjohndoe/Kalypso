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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

public class MainChannelHelper
{
  public static List<GM_Point> getPointsFromCurve( final GM_Curve curve )
  {
    final String crs = curve.getCoordinateSystem();

    final List<GM_Point> pointList = new ArrayList<GM_Point>();
    try
    {
      final GM_Position[] positions = curve.getAsLineString().getPositions();
      for( final GM_Position position : positions )
      {
        final GM_Point point = GeometryFactory.createGM_Point( position, crs );
        pointList.add( point );
      }
      return pointList;
    }

    catch( final GM_Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }

  public static GM_Position[] getPositionsFromCurves( final GM_Curve[] curves, final Map<GM_Position, GM_Curve> map )
  {
    final List<GM_Position> posList = new ArrayList<GM_Position>();

    for( final GM_Curve curve : curves )
    {
      try
      {
        final GM_Position[] positions = curve.getAsLineString().getPositions();
        for( int i = 0; i < positions.length; i++ )
        {
          posList.add( positions[i] );
          map.put( positions[i], curve );
        }
      }
      catch( final GM_Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
        return null;
      }
    }
    return posList.toArray( new GM_Position[posList.size()] );
  }

}
