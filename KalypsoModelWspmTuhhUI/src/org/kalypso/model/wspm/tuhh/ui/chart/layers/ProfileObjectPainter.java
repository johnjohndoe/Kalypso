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
import java.util.List;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.commons.java.lang.Doubles;
import org.kalypso.model.wspm.core.gml.classifications.IPartType;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecords;
import org.kalypso.model.wspm.ui.view.chart.ProfileStyleUtils;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.util.PolygonExtracter;

import de.openali.odysseus.chart.ext.base.layer.HoverIndex;
import de.openali.odysseus.chart.framework.model.data.DataRange;
import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.figure.IFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.IDefaultStyles;
import de.openali.odysseus.chart.framework.model.figure.impl.PointFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;
import de.openali.odysseus.chart.framework.model.style.IStyle;
import de.openali.odysseus.chart.framework.model.style.impl.LineStyle;

/**
 * Helper for a chart layer that paints profile objects.
 * 
 * @author Gernot Belger
 */
public class ProfileObjectPainter
{
  private final PartTypeAccessor m_partInfo;

  private ICoordinateMapper m_coordinateMapper;

  /**
   * TODO: Should be for each style, so that the order can be defined. Link this, it is not clear what wins.
   */
  private HoverIndex m_hoverIndex;

  private final IFigure< ? extends IStyle>[] m_figures;

  private final IProfileObjectInfoBuilder m_infoBuilder;

  public ProfileObjectPainter( final PartTypeAccessor partInfo, final IProfileObjectInfoBuilder infoBuilder )
  {
    m_partInfo = partInfo;
    m_infoBuilder = infoBuilder;

    m_figures = createFigures();
  }

  private IFigure<IStyle>[] createFigures( )
  {
    final Collection<IFigure< ? extends IStyle>> figures = new ArrayList<>();

    final IPartType partType = m_partInfo.getPartType();
    if( partType == null )
    {
      // HINT: This is for 1D2D, because there no classifications are available (hence no part type).
      // HINT: So we must create a default style here.
      final ILineStyle defaultStyle = new LineStyle( 2, new RGB( 153, 0, 255 ), IDefaultStyles.DEFAULT_ALPHA, IDefaultStyles.DEFAULT_DASHOFFSET, IDefaultStyles.DEFAULT_DASHARRAY, IDefaultStyles.DEFAULT_LINEJOIN, IDefaultStyles.DEFAULT_LINECAP, IDefaultStyles.DEFAULT_MITERLIMIT, IDefaultStyles.DEFAULT_VISIBILITY );
      figures.add( new PolylineFigure( defaultStyle ) );
    }

    final IStyle[] styles = m_partInfo.getStyles();
    for( final IStyle style : styles )
    {
      if( style instanceof IPointStyle )
        figures.add( new PointFigure( (IPointStyle)style ) );
      else if( style instanceof ILineStyle )
        figures.add( new PolylineFigure( (ILineStyle)style ) );
    }

    return figures.toArray( new IFigure[figures.size()] );
  }

  public void setCoordinateMapper( final ICoordinateMapper coordinateMapper )
  {
    m_coordinateMapper = coordinateMapper;
  }

  public void paintLegend( final GC gc, final Point size )
  {
    final Point legendPointPoint = new Point( size.x / 2, size.y / 2 );
    final Point[] legendLinePoints = new Point[6];
    legendLinePoints[0] = new Point( 0, size.x / 2 );
    legendLinePoints[1] = new Point( size.x / 5, size.y / 2 );
    legendLinePoints[2] = new Point( size.x / 5 * 2, size.y / 4 );
    legendLinePoints[3] = new Point( size.x / 5 * 3, size.y / 4 * 3 );
    legendLinePoints[4] = new Point( size.x / 5 * 4, size.y / 2 );
    legendLinePoints[5] = new Point( size.x, size.y / 2 );

    for( final IFigure< ? extends IStyle> figure : m_figures )
    {
      if( figure instanceof PointFigure )
        paintPoint( gc, legendPointPoint, (PointFigure)figure );
      else if( figure instanceof PolylineFigure )
        paintLine( gc, legendLinePoints, (PolylineFigure)figure );
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
    final Double max = range.getMax();

    final double newMin = min == null ? value : Math.min( min, value );
    final double newMax = max == null ? value : Math.max( max, value );

    return new DataRange<>( newMin, newMax );
  }

  public void paint( final GC gc, final IProfileObject object )
  {
    if( m_coordinateMapper == null )
      return;

    /* collect points */
    final ProfileObjectPaintData[] paintPoints = buildPaintPoints( object );

    /* paint points */
    for( final IFigure< ? extends IStyle> figure : m_figures )
    {
      if( figure instanceof PointFigure )
        paintPoints( gc, paintPoints, (PointFigure)figure, object );
      else if( figure instanceof PolylineFigure )
        paintLine( gc, paintPoints, (PolylineFigure)figure, object );
    }
  }

  private void paintPoints( final GC gc, final ProfileObjectPaintData[] paintPoints, final PointFigure figure, final IProfileObject object )
  {
    /* create hover figure */
    final IPointStyle style = figure.getStyle();
    final IPointStyle hoverStyle = ProfileStyleUtils.deriveHoverStyle( style );

    /* paint each point and create info */
    for( final ProfileObjectPaintData paintPoint : paintPoints )
    {
      final Point point = paintPoint.getPoint();
      paintPoint( gc, point, figure );

      final PointFigure hoverFigure = new PointFigure( hoverStyle );
      final Rectangle hoverRect = hoverFigure.setCenterPoint( point );

      final EditInfo info = m_infoBuilder.createPointInfo( object, paintPoint.getRecord(), hoverFigure, null );

      m_hoverIndex.addElement( hoverRect, info );
    }
  }

  private void paintLine( final GC gc, final ProfileObjectPaintData[] paintPoints, final PolylineFigure figure, final IProfileObject object )
  {
    /* extract points */
    final Point[] points = new Point[paintPoints.length];
    for( int i = 0; i < points.length; i++ )
      points[i] = paintPoints[i].getPoint();

    /* paint line */
    paintLine( gc, points, figure );

    /* hover figure */
    final ILineStyle style = figure.getStyle();
    final ILineStyle hoverStyle = ProfileStyleUtils.deriveHoverStyle( style );

    /* hover bounds */
    final Polygon hoverBounds = buildHoverBounds( points, style.getWidth() + 1 );

    /* edit info */
    if( hoverBounds != null )
    {
      final PolylineFigure hoverFigure = new PolylineFigure( hoverStyle );
      hoverFigure.setPoints( points );

      final EditInfo info = m_infoBuilder.createLineInfo( object, hoverFigure );
      m_hoverIndex.addElement( hoverBounds, info );
    }
  }

  private Polygon buildHoverBounds( final Point[] points, final int bufferSize )
  {
    final CoordinateList crds = new CoordinateList();

    for( final Point point : points )
      crds.add( new Coordinate( point.x, point.y ), false );

    final Coordinate[] crdArray = crds.toCoordinateArray();
    if( crdArray.length < 2 )
      return null;

    final LineString line = JTSAdapter.jtsFactory.createLineString( crdArray );
    final Geometry buffer = line.buffer( bufferSize );

    final List<Polygon> polygons = PolygonExtracter.getPolygons( buffer );
    if( polygons.size() == 0 )
      return null;

    return polygons.get( 0 );
  }

  private ProfileObjectPaintData[] buildPaintPoints( final IProfileObject object )
  {
    final Collection<ProfileObjectPaintData> paintData = new ArrayList<>();

    final IProfileObjectRecords records = object.getRecords();
    for( int i = 0; i < records.size(); i++ )
    {
      final IProfileObjectRecord record = records.getRecord( i );

      final Point screen = toScreen( record );
      if( screen != null )
      {
        final ProfileObjectPaintData point = new ProfileObjectPaintData( record, screen );
        paintData.add( point );
      }
    }

    return paintData.toArray( new ProfileObjectPaintData[paintData.size()] );
  }

  private Point toScreen( final IProfileObjectRecord record )
  {
    final Double x = record.getBreite();
    final Double y = record.getHoehe();

    if( Doubles.isNaN( x, y ) )
      return null;

    return m_coordinateMapper.numericToScreen( x, y );
  }

  private Rectangle paintPoint( final GC gc, final Point point, final PointFigure figure )
  {
    final Rectangle hoverRect = figure.setCenterPoint( point );
    figure.paint( gc );
    return hoverRect;
  }

  private void paintLine( final GC gc, final Point[] points, final PolylineFigure figure )
  {
    figure.setPoints( points );
    figure.paint( gc );
  }

  public void setHoverIndex( final HoverIndex hoverIndex )
  {
    m_hoverIndex = hoverIndex;
  }
}