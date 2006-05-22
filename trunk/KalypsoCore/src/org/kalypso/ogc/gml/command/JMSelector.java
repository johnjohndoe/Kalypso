/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.command;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Finds features in the map
 * 
 * @author von D�mming
 */
public class JMSelector
{
  /**
   * // selects all features (display elements) that are located within the submitted bounding box. // GMLGeometry
   * gmlGeometry=GMLFactory.createGMLGeometry(bbox); //Operation operation=new
   * SpatialOperation(OperationDefines.WITHIN,myPropertyName,gmlGeometry); //Filter filter=new ComplexFilter(operation);
   */
  public List<Feature> select( final GM_Envelope env, final FeatureList list, final boolean selectWithinBoxStatus )
  {
    try
    {
      final List<Feature> testFE = new ArrayList<Feature>();

      final List<Feature> features = list == null ? new ArrayList<Feature>() : list.query( env, new ArrayList<Feature>() );
      final Iterator containerIterator = features.iterator();

      while( containerIterator.hasNext() )
      {
        final Feature fe = (Feature) containerIterator.next();

        final GM_Object defaultGeometryProperty = fe.getDefaultGeometryProperty();
        if( defaultGeometryProperty != null )
        {

          final CS_CoordinateSystem coordinateSystem = defaultGeometryProperty.getCoordinateSystem();

          final GM_Surface bbox = GeometryFactory.createGM_Surface( env, coordinateSystem );

          if( (selectWithinBoxStatus && bbox.contains( defaultGeometryProperty )) || (!selectWithinBoxStatus && bbox.intersects( defaultGeometryProperty )) )
            testFE.add( fe );
        }
      }

      return testFE;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return new ArrayList<Feature>();
  }

  /**
   * selects all features that intersects the submitted point
   */
  public List<Feature> select( final GM_Position position, final FeatureList list )
  {
    final List<Feature> resultList = new ArrayList<Feature>();
    final List<Feature> testFe = new ArrayList<Feature>();

    final List<Feature> features = list.query( position, new ArrayList<Feature>() );
    for( Feature feature : features )
    {
      try
      {
        if( feature.getDefaultGeometryProperty().contains( position ) )
          testFe.add( feature );
      }
      catch( Exception err )
      {
        System.out.println( err.getMessage() );
        System.out.println( "...using workaround \"box selection\"" );
        System.out.println( "set view dependent radius" );
        resultList.addAll( select( position, 0.0001d, list, false ) );
      }
    }

    resultList.addAll( testFe );

    return resultList;
  }

  /**
   * selects all features (display elements) that are located within the circle described by the position and the
   * radius.
   */
  public List<Feature> select( final GM_Position pos, double r, final FeatureList list, boolean withinStatus )
  {
    final GM_Envelope env = GeometryFactory.createGM_Envelope( pos.getX() - r, pos.getY() - r, pos.getX() + r, pos.getY() + r );
    final List<Feature> resultDE = select( env, list, withinStatus );

    return resultDE;
  }

  public Feature selectNearest( GM_Point pos, final double r, final FeatureList list, final boolean withinStatus )
  {
    Feature result = null;
    double dist = 0;
    final List listFE = select( pos.getPosition(), r, list, withinStatus );
    for( int i = 0; i < listFE.size(); i++ )
    {
      final Feature fe = (Feature) listFE.get( i );

      // TODO: ich bin der Meinung das ist bloedsinn, Gernot
      // TODO: nachtrag: es konnte auch bisher nicht richtig funktionierne,
      // weil deegree die distance nicht implementiert hat!
      final GM_Object defaultGeometryProperty = fe.getDefaultGeometryProperty();
      double distance = defaultGeometryProperty.distance( pos );
      // some geometries must be prefered, otherwise it is not possible to
      // select a point inside a polygon as the polygone is always more near
      if( defaultGeometryProperty instanceof GM_Surface )
        distance += 2 * r / 5;
      if( defaultGeometryProperty instanceof GM_Curve )
        distance += r / 5;
      if( result == null || distance < dist )
      {
        result = fe;
        dist = distance;
      }
    }
    return result;
  }

  public GM_Position selectNearestHandel( GM_Object geom, GM_Position pos, double snapRadius )
  {
    GM_Position[] positions = null;
    try
    {
      if( geom instanceof GM_Surface )
      {
        GM_Surface surface = (GM_Surface) geom;
        positions = surface.getSurfaceBoundary().getExteriorRing().getPositions();
      }
      else if( geom instanceof GM_Curve )
      {
        GM_Curve curve = (GM_Curve) geom;
        positions = curve.getAsLineString().getPositions();
      }
      else if( geom instanceof GM_Point )
      {
        GM_Point point = (GM_Point) geom;
        positions = new GM_Position[] { GeometryFactory.createGM_Position( point.getX(), point.getY() ) };
      }
    }
    catch( GM_Exception e )
    {
      e.printStackTrace();
      // do nothing else
    }
    for( int i = 0; i < positions.length; i++ )
    {
      GM_Position position = positions[i];
      if( position.getDistance( pos ) <= snapRadius )
        return position;
    }
    return null;
  }
}