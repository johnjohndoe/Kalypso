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
package org.kalypso.ogc.gml.widgets;

import java.awt.Color;
import java.awt.Graphics;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.NotImplementedException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.ogc.gml.widgets.tools.AdvancedEditWidgetHelper;
import org.kalypso.ogc.gml.widgets.tools.AdvancedEditWidgetSnapper;
import org.kalypso.ogc.gml.widgets.tools.GeometryPainter;
import org.kalypso.ogc.gml.widgets.tools.IPointHighLighter;
import org.kalypso.ogc.gml.widgets.tools.ISnappedPoint;
import org.kalypso.ogc.gml.widgets.tools.AdvancedEditWidgetHelper.DIRECTION;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * @author Dirk Kuch
 */
public class AdvancedPolygonEditWidget extends AbstractKeyListenerWidget
{
  private static final Color POLYGON_FILL = new Color( 0xa3, 0xc3, 0xc9, 80 );

  private static final Color POLYGON_BORDER = new Color( 255, 255, 255 );

  private static final IPointHighLighter VERTEX = new IPointHighLighter()
  {
    Color cVertex = new Color( 0x3e, 0x79, 0xd9 );

    @Override
    public void draw( final Graphics g, final java.awt.Point point )
    {
      final Color original = g.getColor();
      g.setColor( cVertex );
      g.drawRect( point.x - 6 / 2, point.y - 6 / 2, 6, 6 );
      g.setColor( original );
    }
  };

  private static final IPointHighLighter SNAP = new IPointHighLighter()
  {
    Color cSnap = new Color( 0x40, 0xde, 0x28 );

    int size = 10;

    @Override
    public void draw( final Graphics g, final java.awt.Point point )
    {
      final Color original = g.getColor();
      g.setColor( cSnap );
      g.fillOval( point.x - size / 2, point.y - size / 2, size, size );
      g.setColor( original );
    }
  };

  private static final IPointHighLighter MOVED_SNAP_POINT = new IPointHighLighter()
  {
    Color cSnap = new Color( 0x31, 0x47, 0xa0, 128 );

    int size = 5;

    @Override
    public void draw( final Graphics g, final java.awt.Point point )
    {
      final Color original = g.getColor();
      g.setColor( cSnap );
      g.fillOval( point.x - size / 2, point.y - size / 2, size, size );
      g.setColor( original );
    }
  };

  private final IAdvancedEditWidgetDataProvider m_provider;

  private Point m_originPoint = null;

  private ISnappedPoint[] m_snappedPointsAtOrigin = null;

  public AdvancedPolygonEditWidget( final IAdvancedEditWidgetDataProvider provider )
  {
    super( "Editiere st‰dtebauliche Elemente" );
    m_provider = provider;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    final GM_Point gmp = getCurrentGmPoint();
    if( gmp == null )
      return;
    try
    {
      final Point jtsPoint = (Point) JTSAdapter.export( gmp );

      final Feature[] features = m_provider.query( gmp, 20 );
      if( ArrayUtils.isEmpty( features ) )
        return;

      // highlight detected feature points
      final Map<Geometry, Feature> mapGeometries = m_provider.resolveJtsGeometries( features );
      GeometryPainter.highlightPoints( g, getMapPanel(), mapGeometries.keySet().toArray( new Geometry[] {} ), VERTEX );

      // find snap points
      final ISnappedPoint[] snappedPoints = AdvancedEditWidgetSnapper.findSnapPoints( mapGeometries, jtsPoint, getRange() );
      if( !ArrayUtils.isEmpty( snappedPoints ) )
      {
        final Set<Point> snapped = new LinkedHashSet<Point>();
        for( final ISnappedPoint p : snappedPoints )
        {
          snapped.add( p.getPoint() );
        }

        GeometryPainter.highlightPoints( g, getMapPanel(), snapped.toArray( new Geometry[] {} ), SNAP );
      }

      // drag & drop symbolization
      if( m_originPoint != null )
      {
        final Point vector = JTSUtilities.getVector( m_originPoint, jtsPoint );
        final Map<Feature, ISnappedPoint[]> snapped = resolveSnappedPoints();

        /* highlight moved snap points */
        final Set<Point> moved = new HashSet<Point>();

        final Set<Entry<Feature, ISnappedPoint[]>> entrySet = snapped.entrySet();
        for( final Entry<Feature, ISnappedPoint[]> entry : entrySet )
        {
          final ISnappedPoint[] points = entry.getValue();
          for( final ISnappedPoint p : points )
          {
            moved.add( p.getMovedPoint( vector ) );
          }
        }

        GeometryPainter.highlightPoints( g, getMapPanel(), moved.toArray( new Geometry[] {} ), MOVED_SNAP_POINT );

        /* display new geometry */
        displayUpdateGeometry( g, snapped, vector );
      }

    }
    catch( final GM_Exception e )
    {
      KalypsoCorePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  private void displayUpdateGeometry( final Graphics g, final Map<Feature, ISnappedPoint[]> map, final Point vector )
  {
    final Set<Polygon> results = new HashSet<Polygon>();

    final Set<Entry<Feature, ISnappedPoint[]>> entries = map.entrySet();
    for( final Entry<Feature, ISnappedPoint[]> entry : entries )
    {
      final Feature feature = entry.getKey();

      final Geometry geometry = m_provider.resolveJtsGeometry( feature );
      final ISnappedPoint[] points = entry.getValue();

      if( geometry instanceof Polygon && points.length == 1 )
      {
        final Polygon polygon = (Polygon) geometry;
        final ISnappedPoint point = points[0];

        final LineString ring = polygon.getExteriorRing();
        final int indexCurrent = AdvancedEditWidgetHelper.getIndexOfPoint( ring, point.getPoint(), DIRECTION.eForward );
       
        final Point moved = point.getMovedPoint( vector );

        final Polygon result = AdvancedEditWidgetHelper.resolveResultPolygon( polygon, indexCurrent, moved );
        results.add( result );
      }
      else if( geometry instanceof Polygon && points.length > 1 )
      {
        final Polygon polygon = (Polygon) geometry;
        final ISnappedPoint firstPoint = points[0];
        final ISnappedPoint lastPoint = points[points.length - 1];

        final LineString ring = polygon.getExteriorRing();
        final int indexFirst = AdvancedEditWidgetHelper.getIndexOfPoint( ring, firstPoint.getPoint(), DIRECTION.eForward );
        final int indexLast = AdvancedEditWidgetHelper.getIndexOfPoint( ring, lastPoint.getPoint(), DIRECTION.eBackward );

        final int indexPrevious = AdvancedEditWidgetHelper.resolveNeighbor( ring, ring.getPointN( indexFirst ), indexFirst, DIRECTION.eBackward );
        final int indexNext = AdvancedEditWidgetHelper.resolveNeighbor( ring, ring.getPointN( indexLast ), indexLast, DIRECTION.eForward );

        final Polygon result = AdvancedEditWidgetHelper.resolveResultPolygon( polygon, indexPrevious, indexNext, vector );
        results.add( result );
      }
      else
        throw new NotImplementedException();
    }

    GeometryPainter.drawPolygons( getMapPanel(), g, results.toArray( new Polygon[] {} ), POLYGON_BORDER, POLYGON_FILL );
  }

  private Map<Feature, ISnappedPoint[]> resolveSnappedPoints( )
  {
    final Map<Feature, ISnappedPoint[]> map = new HashMap<Feature, ISnappedPoint[]>();

    for( final ISnappedPoint point : m_snappedPointsAtOrigin )
    {
      final Feature feature = point.getFeature();

      final ISnappedPoint[] points = map.get( feature );
      if( points == null )
      {
        map.put( feature, new ISnappedPoint[] { point } );
      }
      else
      {
        map.put( feature, (ISnappedPoint[]) ArrayUtils.add( points, point ) );
      }
    }

    return map;
  }

  private double getRange( )
  {
    final double scale = getMapPanel().getCurrentScale();
    final double range = scale / 4;

    return range;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#leftPressed(java.awt.Point)
   */
  @Override
  public void leftPressed( final java.awt.Point p )
  {

    final GM_Point gmp = getCurrentGmPoint();
    if( gmp == null )
      return;

    try
    {
      m_originPoint = (Point) JTSAdapter.export( gmp );

      final Feature[] features = m_provider.query( gmp, 20 );
      if( ArrayUtils.isEmpty( features ) )
        return;

      // highlight detected feature points
      final Map<Geometry, Feature> mapGeometries = m_provider.resolveJtsGeometries( features );

      m_snappedPointsAtOrigin = AdvancedEditWidgetSnapper.findSnapPoints( mapGeometries, m_originPoint, getRange() );

    }
    catch( final GM_Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#leftReleased(java.awt.Point)
   */
  @Override
  public void leftReleased( final java.awt.Point p )
  {

    m_originPoint = null;
    m_snappedPointsAtOrigin = null;
  }
}
