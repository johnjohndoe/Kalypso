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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.kalypso.commons.java.lang.Doubles;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecords;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer;

import de.openali.odysseus.chart.framework.model.data.DataRange;
import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.figure.impl.PointFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;
import de.openali.odysseus.chart.framework.model.layer.impl.LegendEntry;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;
import de.openali.odysseus.chart.framework.model.style.IStyle;
import de.openali.odysseus.chart.framework.util.img.ChartImageInfo;

/**
 * Layer that renders the records of a {@link org.kalypso.model.wspm.core.profil.IProfileObject}.
 * 
 * @author Gernot Belger
 */
public class ProfileObjectsLayer extends AbstractProfilLayer
{
  private final IProfileObject m_object;

  private final PartTypeAccessor m_partInfo;

  public ProfileObjectsLayer( final String id, final IProfile profil, final IProfileObject object, final String title )
  {
    super( id, profil );

    m_object = object;

    m_partInfo = new PartTypeAccessor( profil, object.getType() );

    final String label = m_partInfo.getTypeLabel();

    final String typeTitle = String.format( "%s %s", label, m_object.getDescription() );

    if( title == null )
      setTitle( typeTitle );
    else
      setTitle( title );
  }

  @Override
  public synchronized ILegendEntry[] getLegendEntries( )
  {
    final ILegendEntry entry = new LegendEntry( this, getTitle() )
    {
      @Override
      public void paintSymbol( final GC gc, final Point size )
      {
        paintLegendSymbol( gc, size );
      }
    };

    return new ILegendEntry[] { entry };
  }

  protected void paintLegendSymbol( final GC gc, final Point size )
  {
    final IStyle[] styles = m_partInfo.getStyles();
    final Point[] legendPointPoints = new Point[] { new Point( size.x / 2, size.y / 2 ) };
    final Point[] legendLinePoints = new Point[6];
    legendLinePoints[0] = new Point( 0, size.x / 2 );
    legendLinePoints[1] = new Point( size.x / 5, size.y / 2 );
    legendLinePoints[2] = new Point( size.x / 5 * 2, size.y / 4 );
    legendLinePoints[3] = new Point( size.x / 5 * 3, size.y / 4 * 3 );
    legendLinePoints[4] = new Point( size.x / 5 * 4, size.y / 2 );
    legendLinePoints[5] = new Point( size.x, size.y / 2 );

    for( final IStyle style : styles )
    {
      if( style instanceof IPointStyle )
        paintPoints( gc, legendPointPoints, (IPointStyle)style );
      else if( style instanceof ILineStyle )
        paintLine( gc, legendLinePoints, (ILineStyle)style );
    }
  }

  @Override
  public IDataRange<Double> getDomainRange( )
  {
    DataRange<Double> range = new DataRange<>( null, null );

    final IProfileObjectRecords records = m_object.getRecords();
    for( int i = 0; i < records.size(); i++ )
    {
      final IProfileObjectRecord record = records.getRecord( i );
      final Double breite = record.getBreite();

      range = extendRange( range, breite );
    }

    return range;
  }

  @Override
  public IDataRange<Double> getTargetRange( final IDataRange domainRange )
  {
    DataRange<Double> range = new DataRange<>( null, null );

    final IProfileObjectRecords records = m_object.getRecords();
    for( int i = 0; i < records.size(); i++ )
    {
      final IProfileObjectRecord record = records.getRecord( i );
      final Double hoehe = record.getHoehe();

      range = extendRange( range, hoehe );
    }

    return range;
  }

  private DataRange<Double> extendRange( final DataRange<Double> range, final Double value )
  {
    if( value == null )
      return range;

    final Double min = range.getMin();
    final Double max = range.getMin();

    final double newMin = min == null ? value : Math.min( min, value );
    final double newMax = max == null ? value : Math.min( max, value );

    return new DataRange<>( newMin, newMax );
  }

  @Override
  public void paint( final GC gc, final ChartImageInfo chartImageInfo, final IProgressMonitor monitor )
  {
    /* collect points */
    final Point[] points = buildPaintPoints();

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

  private Point[] buildPaintPoints( )
  {
    final Collection<Point> points = new ArrayList<>();

    final IProfileObjectRecords records = m_object.getRecords();
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
    final ICoordinateMapper cm = getCoordinateMapper();
    if( Objects.isNull( cm ) )
      return null;

    final Double x = record.getBreite();
    final Double y = record.getHoehe();

    if( Doubles.isNaN( x, y ) )
      return null;

    return cm.numericToScreen( x, y );
  }

  @Override
  public EditInfo getHover( final Point pos )
  {
    return null;
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

  @Override
  public void executeDrop( final Point point, final EditInfo dragStartData )
  {
  }

  @Override
  public void executeClick( final EditInfo dragStartData )
  {
  }

  @Override
  public EditInfo drag( final Point newPos, final EditInfo dragStartData )
  {
    return null;
  }

  @Override
  public EditInfo commitDrag( final Point point, final EditInfo dragStartData )
  {
    return null;
  }
}