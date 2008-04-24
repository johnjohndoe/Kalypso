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
package org.kalypso.ogc.gml.map.widgets;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.command.Handle;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfaceBoundary;

/**
 * @author Thomas Jung
 */
public class HandlesFactory
{

  public static List<Handle> createHandles( final List<Object> features, List<Handle> collector, final GM_Envelope envelope )
  {
    if( collector == null )
      collector = new ArrayList<Handle>();
    for( final Object feature : features )
      createHandles( (Feature) feature, collector, envelope );
    return collector;
  }

  private static List<Handle> createHandles( final Feature feature, final List<Handle> collector, final GM_Envelope envelope )
  {
    final IValuePropertyType[] geometryProperties = feature.getFeatureType().getAllGeomteryProperties();
    for( final IValuePropertyType propType : geometryProperties )
    {
      final GM_Object geometry = (GM_Object) feature.getProperty( propType );
      createHandles( feature, propType, geometry, collector, envelope );
    }
    return collector;
  }

  @SuppressWarnings("unchecked")
  private static void createHandles( final Feature feature, final IValuePropertyType propType, final GM_Object geometry, final List<Handle> collector, final GM_Envelope envelope )
  {
    if( geometry instanceof GM_Point )
      createPointHandles( feature, propType, (GM_Point) geometry, collector, envelope );
    else if( geometry instanceof GM_Curve )
      createCurveHandles( feature, propType, (GM_Curve) geometry, collector, envelope );
    else if( geometry instanceof GM_Surface )
      createSurfaceHandles( feature, propType, (GM_Surface) geometry, collector, envelope );
  }

  private static void createPointHandles( final Feature feature, final IValuePropertyType propType, final GM_Point point, final List<Handle> collector, final GM_Envelope envelope )
  {
    final GM_Position position = point.getPosition();
    if( envelope.contains( position ) )
      collector.add( new Handle( feature, propType, position ) );
  }

  private static void createCurveHandles( final Feature feature, final IValuePropertyType propType, final GM_Curve curve, final List<Handle> collector, final GM_Envelope envelope )
  {
    final GM_Position[] positions;
    try
    {
      positions = curve.getAsLineString().getPositions();
      createHandles( feature, propType, positions, collector, envelope );
    }
    catch( final GM_Exception e )
    {
      // create no handles
      e.printStackTrace();
    }

  }

  @SuppressWarnings("unchecked")
  private static void createSurfaceHandles( final Feature feature, final IValuePropertyType propType, final GM_Surface surface, final List<Handle> collector, final GM_Envelope envelope )
  {
    final GM_SurfaceBoundary surfaceBoundary = surface.getSurfaceBoundary();
    if( surfaceBoundary == null )
      return;
    final GM_Ring exteriorRing = surfaceBoundary.getExteriorRing();
    final GM_Position[] positionsExterior = exteriorRing.getPositions();
    createHandles( feature, propType, positionsExterior, collector, envelope );
    final GM_Ring[] interiorRings = surfaceBoundary.getInteriorRings();
    if( interiorRings != null )
      for( final GM_Ring ring : interiorRings )
      {
        final GM_Position[] positionsInteror = ring.getPositions();
        createHandles( feature, propType, positionsInteror, collector, envelope );
      }
  }

  private static void createHandles( final Feature feature, final IValuePropertyType propType, final GM_Position[] positions, final List<Handle> collector, final GM_Envelope envelope )
  {
    final Set<GM_Position> pos = new HashSet<GM_Position>();
    for( final GM_Position position : positions )
    {
      if( envelope.contains( position ) )
      {
        if( !pos.contains( position ) )
        {
          collector.add( new Handle( feature, propType, position ) );
          pos.add( position );
        }
      }
    }
  }
}
