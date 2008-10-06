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
package org.kalypso.ogc.gml.map.widgets.providers;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.map.widgets.providers.handles.Handle;
import org.kalypso.ogc.gml.map.widgets.providers.handles.IHandle;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_CurveSegment;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * This class provides handles for line geometries.
 * 
 * @author Holger Albert
 */
public class LineHandlesProvider implements IHandlesProvider
{
  /**
   * @see org.kalypso.informdss.manager.util.widgets.providers.IHandlesProvider#collectHandles(org.kalypsodeegree.model.feature.Feature)
   */
  public List<IHandle> collectHandles( Feature feature, int radius )
  {
    ArrayList<IHandle> list = new ArrayList<IHandle>();

    final IValuePropertyType[] allGeomteryProperties = feature.getFeatureType().getAllGeomteryProperties();
    for( final IValuePropertyType type : allGeomteryProperties )
    {
      try
      {
        final GM_Object object = (GM_Object) feature.getProperty( type );

        /* Only handles for a line are returned. */
        if( object instanceof GM_Curve )
        {
          GM_Curve curve = (GM_Curve) object;
          int numberOfCurveSegments = curve.getNumberOfCurveSegments();

          for( int i = 0; i < numberOfCurveSegments; i++ )
          {
            /* One segment of a curve. It can contain several points. */
            GM_CurveSegment curveSegment = curve.getCurveSegmentAt( i );

            GM_Position[] positions = curveSegment.getPositions();

            for( GM_Position position : positions )
            {
              /* Add the points of every segment to the list of handles. */
              list.add( new Handle( position, feature, type, radius ) );
            }
          }
        }
      }
      catch( GM_Exception e )
      {
        // TODO
      }
    }

    return list;
  }
}
