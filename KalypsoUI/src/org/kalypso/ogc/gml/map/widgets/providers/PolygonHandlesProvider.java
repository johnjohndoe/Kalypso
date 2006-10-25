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
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * This class provides handles for polygon geometries.
 * 
 * @author Holger Albert
 */
public class PolygonHandlesProvider implements IHandlesProvider
{
  /**
   * @see org.kalypso.informdss.manager.util.widgets.providers.IHandlesProvider#collectHandles(org.kalypsodeegree.model.feature.Feature)
   */
  public List<IHandle> collectHandles( Feature feature, int radius )
  {
    ArrayList<IHandle> list = new ArrayList<IHandle>();

    final IValuePropertyType[] allGeomteryProperties = feature.getFeatureType().getAllGeomteryProperties();

    for( final IValuePropertyType geoVpt : allGeomteryProperties )
    {
      final GM_Object gmObj = (GM_Object) feature.getProperty( geoVpt );
      try
      {
        if( GeometryUtilities.isPolygonGeometry( geoVpt ) )
        {
          GM_Surface surface = (GM_Surface) gmObj;

          int numberOfSurfacePatches = surface.getNumberOfSurfacePatches();

          for( int i = 0; i < numberOfSurfacePatches; i++ )
          {
            /* One patch of a surface. It can contain several points. */
            GM_SurfacePatch surfacePatch = surface.getSurfacePatchAt( i );

            GM_Position[] exteriorRing = surfacePatch.getExteriorRing();
            for( GM_Position position : exteriorRing )
            {
              /* Add the points to the list of handles. */
              list.add( new Handle( position, feature, geoVpt, radius ) );
            }

            GM_Position[][] interiorRings = surfacePatch.getInteriorRings();

            for( GM_Position[] positions : interiorRings )
            {
              for( GM_Position position : positions )
              {
                /* Add the points to the list of handles. */
                list.add( new Handle( position, feature, geoVpt, radius ) );
              }
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