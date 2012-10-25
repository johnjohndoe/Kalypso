/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.chart.layers;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.kalypso.commons.java.lang.Doubles;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecords;

import de.openali.odysseus.chart.ext.base.layer.HoverIndex;
import de.openali.odysseus.chart.framework.model.data.DataRange;
import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.figure.impl.PointFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;
import de.openali.odysseus.chart.framework.model.style.IStyle;

/**
 * Helper for a chart layer that paints profile objects.
 * 
 * @author Gernot Belger
 */
public class ProfileObjectPainter
{
  private final PartTypeAccessor m_partInfo;

  private ICoordinateMapper m_coordinateMapper;

  private HoverIndex m_hoverIndex;

  public ProfileObjectPainter( final IProfile profile, final String type )
  {
    m_partInfo = new PartTypeAccessor( profile, type );
  }

  public void setCoordinateMapper( final ICoordinateMapper coordinateMapper )
  {
    m_coordinateMapper = coordinateMapper;
  }

  public PartTypeAccessor getInfo( )
  {
    return m_partInfo;
  }

  public void paintLegend( final GC gc, final Point size )
  {
    final Point[] legendPointPoints = new Point[] { new Point( size.x / 2, size.y / 2 ) };
    final Point[] legendLinePoints = new Point[6];
    legendLinePoints[0] = new Point( 0, size.x / 2 );
    legendLinePoints[1] = new Point( size.x / 5, size.y / 2 );
    legendLinePoints[2] = new Point( size.x / 5 * 2, size.y / 4 );
    legendLinePoints[3] = new Point( size.x / 5 * 3, size.y / 4 * 3 );
    legendLinePoints[4] = new Point( size.x / 5 * 4, size.y / 2 );
    legendLinePoints[5] = new Point( size.x, size.y / 2 );

    final IStyle[] styles = m_partInfo.getStyles();
    for( final IStyle style : styles )
    {
      if( style instanceof IPointStyle )
        paintPoints( gc, legendPointPoints, (IPointStyle)style );
      else if( style instanceof ILineStyle )
        paintLine( gc, legendLinePoints, (ILineStyle)style );
    }
  }

  public IDataRange<Double> getDomainRange( final IProfileObject... objects )
  {
    DataRange<Double> range = new DataRange<>( null, null );

    for( final IProfileObject object : objects )
    {
      final IProfileObjectRecords records = object.getRecords();
      for( int i = 0; i < records.size(); i++ )
      {
        final IProfileObjectRecord record = records.getRecord( i );
        final Double breite = record.getBreite();

        range = extendRange( range, breite );
      }
    }

    return range;
  }

  public IDataRange<Double> getTargetRange( final IProfileObject... objects )
  {
    DataRange<Double> range = new DataRange<>( null, null );

    for( final IProfileObject object : objects )
    {
      final IProfileObjectRecords records = object.getRecords();
      for( int i = 0; i < records.size(); i++ )
      {
        final IProfileObjectRecord record = records.getRecord( i );
        final Double hoehe = record.getHoehe();

        range = extendRange( range, hoehe );
      }
    }

    return range;
  }

  private static DataRange<Double> extendRange( final DataRange<Double> range, final Double value )
  {
    if( value == null )
      return range;

    final Double min = range.getMin();
    final Double max = range.getMin();

    final double newMin = min == null ? value : Math.min( min, value );
    final double newMax = max == null ? value : Math.min( max, value );

    return new DataRange<>( newMin, newMax );
  }

  public void paint( final GC gc, final IProfileObject object )
  {
    if( m_coordinateMapper == null )
      return;

    /* collect points */
    final Point[] points = buildPaintPoints( object );

    /* paint points */
    final IStyle[] styles = m_partInfo.getStyles();
    for( final IStyle style : styles )
      paint( gc, points, style );
  }

  private void paint( final GC gc, final Point[] points, final IStyle style )
  {
    if( style instanceof IPointStyle )
      paintPoints( gc, points, (IPointStyle)style );
    else if( style instanceof ILineStyle )
      paintLine( gc, points, (ILineStyle)style );
  }

  private Point[] buildPaintPoints( final IProfileObject object )
  {
    final Collection<Point> points = new ArrayList<>();

    final IProfileObjectRecords records = object.getRecords();
    for( int i = 0; i < records.size(); i++ )
    {
      final IProfileObjectRecord record = records.getRecord( i );

      final Point screen = toScreen( record );
      if( screen != null )
      {
        points.add( screen );

        // FIXME: add hover info
      }
    }

    return points.toArray( new Point[points.size()] );
  }

  private Point toScreen( final IProfileObjectRecord record )
  {
    final Double x = record.getBreite();
    final Double y = record.getHoehe();

    if( Doubles.isNaN( x, y ) )
      return null;

    return m_coordinateMapper.numericToScreen( x, y );
  }

  private void paintPoints( final GC gc, final Point[] points, final IPointStyle style )
  {
    final PointFigure figure = new PointFigure();
    figure.setStyle( style );
    figure.setPoints( points );
    figure.paint( gc );
  }

  private void paintLine( final GC gc, final Point[] points, final ILineStyle lineStyle )
  {
    final PolylineFigure lineFigure = new PolylineFigure();
    lineFigure.setStyle( lineStyle );
    lineFigure.setPoints( points );
    lineFigure.paint( gc );
  }

  public void setHoverIndex( final HoverIndex hoverIndex )
  {
    m_hoverIndex = hoverIndex;
  }
}