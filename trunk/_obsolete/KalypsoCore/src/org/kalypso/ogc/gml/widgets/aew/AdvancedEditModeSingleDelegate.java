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
package org.kalypso.ogc.gml.widgets.aew;

import java.awt.Graphics;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.ogc.gml.widgets.tools.GeometryPainter;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

/**
 * @author Dirk Kuch
 */
public class AdvancedEditModeSingleDelegate extends AbstractAdvancedEditModeMovementDelegate
{
  public AdvancedEditModeSingleDelegate( final IAdvancedEditWidget widget, final IAdvancedEditWidgetDataProvider provider )
  {
    super( widget, provider );
  }

  public void paint( final Graphics g )
  {
    final GM_Point gmp = getWidget().getCurrentGmPoint();
    if( gmp == null )
      return;
    try
    {
      final Point jtsPoint = (Point) JTSAdapter.export( gmp );
      
      final IAdvancedEditWidgetSnappedPoint[] snappedPoints;
      
      if( getWidget().isLeftMouseButtonPressed() )
      {
        snappedPoints = filterSnapPoints( getWidget().getSnappedPointsAtOrigin() );
      }
      else
      {
        final Feature[] features = getProvider().query( gmp, 20 );
        if( ArrayUtils.isEmpty( features ) )
          return;

        // highlight detected feature points
        final Map<Geometry, Feature> mapGeometries = getProvider().resolveJtsGeometries( features );
        GeometryPainter.highlightPoints( g, getWidget().getIMapPanel(), mapGeometries.keySet().toArray( new Geometry[] {} ), VERTEX );

        // find snap points
        snappedPoints = filterSnapPoints( AdvancedEditWidgetSnapper.findSnapPoints( mapGeometries, jtsPoint, getRange() ) );
      }

      if( !ArrayUtils.isEmpty( snappedPoints ) )
      {
        final Set<Point> snapped = new LinkedHashSet<Point>();
        for( final IAdvancedEditWidgetSnappedPoint p : snappedPoints )
        {
          snapped.add( p.getPoint() );
        }

        GeometryPainter.highlightPoints( g, getWidget().getIMapPanel(), snapped.toArray( new Geometry[] {} ), SNAP );

        // drag & drop symbolization
        if( getWidget().getOriginPoint() != null )
        {
          final Point vector = JTSUtilities.getVector( getWidget().getOriginPoint(), jtsPoint );

          /* highlight moved snap points */
          final Set<Point> moved = new HashSet<Point>();

          for( final IAdvancedEditWidgetSnappedPoint p : snappedPoints )
          {
            moved.add( p.getMovedPoint( vector ) );
          }

          GeometryPainter.highlightPoints( g, getWidget().getIMapPanel(), moved.toArray( new Geometry[] {} ), MOVED_SNAP_POINT );

          /* display new geometry */
          if( !ArrayUtils.isEmpty( snappedPoints ) )
          {
            final Map<Feature, IAdvancedEditWidgetSnappedPoint[]> map = new HashMap<Feature, IAdvancedEditWidgetSnappedPoint[]>();
            map.put( snappedPoints[0].getFeature(), snappedPoints );

            displayUpdateGeometry( g, map, vector );
          }
        }
      }
    }
    catch( final GM_Exception e )
    {
      KalypsoCorePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  /**
   * snap points of one feature
   */
  private IAdvancedEditWidgetSnappedPoint[] filterSnapPoints( final IAdvancedEditWidgetSnappedPoint[] points )
  {
    Feature base = null;
    final Set<IAdvancedEditWidgetSnappedPoint> mySnapPoints = new HashSet<IAdvancedEditWidgetSnappedPoint>();

    for( final IAdvancedEditWidgetSnappedPoint point : points )
    {
      if( base == null )
      {
        base = point.getFeature();
      }

      if( base == point.getFeature() )
      {
        mySnapPoints.add( point );
      }
    }

    return mySnapPoints.toArray( new IAdvancedEditWidgetSnappedPoint[] {} );
  }

}
