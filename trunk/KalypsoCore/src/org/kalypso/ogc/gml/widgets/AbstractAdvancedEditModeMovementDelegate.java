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
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.ogc.gml.widgets.tools.AdvancedEditWidgetHelper;
import org.kalypso.ogc.gml.widgets.tools.GeometryPainter;
import org.kalypso.ogc.gml.widgets.tools.ISnappedPoint;
import org.kalypso.ogc.gml.widgets.tools.AdvancedEditWidgetHelper.DIRECTION;
import org.kalypsodeegree.model.feature.Feature;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * @author kuch
 */
public abstract class AbstractAdvancedEditModeMovementDelegate implements IAdvancedEditWidgetDelegate
{

  private static final Color POLYGON_FILL_VALID = new Color( 0xa3, 0xc3, 0xc9, 0x80 );

  private static final Color POLYGON_FILL_INVALID = new Color( 0xda, 0x39, 0x2f );

  private static final Color POLYGON_BORDER = new Color( 255, 255, 255 );

  private final IAdvancedEditWidget m_widget;

  private final IAdvancedEditWidgetDataProvider m_provider;

  public AbstractAdvancedEditModeMovementDelegate( final IAdvancedEditWidget widget, final IAdvancedEditWidgetDataProvider provider )
  {
    m_widget = widget;
    m_provider = provider;
  }

  protected IAdvancedEditWidget getWidget( )
  {
    return m_widget;
  }

  protected IAdvancedEditWidgetDataProvider getProvider( )
  {
    return m_provider;
  }

  protected void displayUpdateGeometry( final Graphics g, final Map<Feature, ISnappedPoint[]> map, final Point vector )
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

    final Polygon[] polygons = results.toArray( new Polygon[] {} );
    final boolean valid = validResults( polygons );

    final Color fillColor = valid ? POLYGON_FILL_VALID : POLYGON_FILL_INVALID;
    GeometryPainter.drawPolygons( m_widget.getIMapPanel(), g, polygons, POLYGON_BORDER, fillColor );

  }

  private boolean validResults( final Polygon[] polygons )
  {
    for( final Polygon polygon : polygons )
    {
      if( !polygon.isSimple() )
        return false;

      if( !polygon.isValid() )
        return false;
    }

// for( final Polygon p1 : polygons )
// {
//
// for( final Polygon p2 : polygons )
// {
// if( p1 == p2 )
// {
// continue;
// }
//        
// final Geometry intersection = p1.intersection( p2 );
// final double area = intersection.getArea();
// if( intersection instanceof Polygon || intersection instanceof MultiPolygon )
// return false;
// }
//
// }

    return true;
  }

  protected double getRange( )
  {
    final double scale = getWidget().getIMapPanel().getCurrentScale();
    final double range = scale / 4;

    return range;
  }

}
