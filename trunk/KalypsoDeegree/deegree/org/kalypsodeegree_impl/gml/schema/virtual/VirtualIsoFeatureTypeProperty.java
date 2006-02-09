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
package org.kalypsodeegree_impl.gml.schema.virtual;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author doemming
 */
public class VirtualIsoFeatureTypeProperty extends AbstractVirtualPropertyType
{
  private final static String DECORATED_NS = "http://www.opengis.net/gml";

  private double m_iso = 0.1d;

  private final static String PROP_GEOM = DECORATED_NS + ":polygonProperty";


  public VirtualIsoFeatureTypeProperty( )
  {
    super(new QName( "virtual", "iso_lines" ),0,1,GeometryUtilities.getLineStringClass());
  }

  /**
   * @see org.kalypsodeegree_impl.gml.schema.virtual.VirtualFeatureTypeProperty#getVirtuelValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypsodeegree.model.feature.GMLWorkspace)
   */
  public Object getVirtuelValue( Feature feature, GMLWorkspace workspace )
  {
    m_iso = 0.1d;
    final List result = new ArrayList();
    try
    {
      final GM_Surface surface = (GM_Surface) feature.getProperty( PROP_GEOM );
      if( surface == null )
        return null;
      GM_Position[] positions = surface.getSurfaceBoundary().getExteriorRing().getPositions();
      if( positions[0].getAsArray().length < 2 )
        return null;
      CS_CoordinateSystem cs = surface.getCoordinateSystem();
      switch( positions.length )
      {
        case 4: // 3 edges
          createISOFrom3( positions[0], positions[1], positions[2], result, cs );
          break;
        case 5: // 4 edges
          createIsoFrom4( positions[0], positions[1], positions[2], positions[3], cs, result, 3 );
          break;
      }
      if( result.isEmpty() )
        return null;
    }
    catch( Exception e )
    {
      // do nothing
    }
    return GeometryFactory.createGM_MultiCurve( (GM_Curve[]) result.toArray( new GM_Curve[result.size()] ) );
  }

  private void createIsoFrom4( GM_Position pos0, GM_Position pos1, GM_Position pos2, GM_Position pos3, CS_CoordinateSystem cs, List result, int level )
  {
    level--;
    // points
    final GM_Position posA = GeometryUtilities.createGM_PositionAtCenter( pos0, pos1 );
    final GM_Position posC = GeometryUtilities.createGM_PositionAtCenter( pos2, pos3 );
    final GM_Position posE = GeometryUtilities.createGM_PositionAtCenter( posA, posC );
    // uncommend this to show raster:
    // try
    // {
    // result.add( GeometryFactory.createGM_Curve( new
    // GM_Position[]{pos0,pos1,pos2,pos3}, cs ) );
    // }
    // catch( GM_Exception e )
    // {
    // e.printStackTrace();
    // }
    if( level > 0 )
    {
      GM_Position posB = GeometryUtilities.createGM_PositionAtCenter( pos1, pos2 );
      GM_Position posD = GeometryUtilities.createGM_PositionAtCenter( pos3, pos0 );
      createIsoFrom4( pos0, posA, posE, posD, cs, result, level );
      createIsoFrom4( posA, pos1, posB, posE, cs, result, level );
      createIsoFrom4( posE, posB, pos2, posC, cs, result, level );
      createIsoFrom4( posD, posE, posC, pos3, cs, result, level );
    }
    else
    {
      createISOFrom3( pos0, pos1, posE, result, cs );
      createISOFrom3( pos1, pos2, posE, result, cs );
      createISOFrom3( pos2, pos3, posE, result, cs );
      createISOFrom3( pos3, pos0, posE, result, cs );
    }
  }

  private List createISOFrom3( GM_Position p1, GM_Position p2, GM_Position p3, List result, CS_CoordinateSystem cs )
  {

    // check isos ?
    double z1 = p1.getZ();
    double z2 = p2.getZ();
    double z3 = p3.getZ();
    double zmin = Math.min( z1, Math.min( z2, z3 ) );
    double isoMin = zmin - (zmin % m_iso);
    double zmax = Math.max( z1, Math.max( z2, z3 ) );
    double isoMax = zmax;
    if( isoMin <= isoMax )
    {
      for( double iso = isoMin; iso <= isoMax; iso += m_iso )
      {
        GM_Position[] isoPos = createISO( p1, p2, p3, iso );
        if( isoPos != null )
          try
          {
            result.add( GeometryFactory.createGM_Curve( isoPos, cs ) );
          }
          catch( GM_Exception e )
          {
            e.printStackTrace();
          }
      }
    }
    return result;
  }

  private GM_Position[] createISO( GM_Position p1, GM_Position p2, GM_Position p3, double iso )
  {
    GM_Position i1 = GeometryUtilities.getGM_PositionBetweenAtLevel( p1, p2, iso );
    GM_Position i2 = GeometryUtilities.getGM_PositionBetweenAtLevel( p2, p3, iso );
    GM_Position i3 = GeometryUtilities.getGM_PositionBetweenAtLevel( p1, p3, iso );
    if( i1 != null && i2 != null && i3 != null )
    {
      if( i1.getDistance( i2 ) > i2.getDistance( i3 ) )
        return new GM_Position[] { i1, i2 };
      return new GM_Position[] { i2, i3 };
    }

    if( i1 != null && i2 != null )
    {
      return new GM_Position[] { i1, i2 };
    }
    if( i2 != null && i3 != null )
    {
      return new GM_Position[] { i2, i3 };
    }
    if( i1 != null && i3 != null )
    {
      return new GM_Position[] { i1, i3 };
    }
    return null;
  }

}