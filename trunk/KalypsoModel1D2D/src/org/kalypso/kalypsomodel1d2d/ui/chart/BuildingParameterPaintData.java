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
package org.kalypso.kalypsomodel1d2d.ui.chart;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.lang3.ObjectUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import com.google.common.collect.Range;
import com.google.common.collect.Ranges;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

import de.openali.odysseus.chart.ext.base.layer.HoverIndex;
import de.openali.odysseus.chart.ext.base.layer.TooltipFormatter;
import de.openali.odysseus.chart.framework.model.data.DataRange;
import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.figure.impl.PointFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;
import de.openali.odysseus.chart.framework.model.style.IStyleSet;

/**
 * @author Gernot
 */
class BuildingParameterPaintData
{
  private final GeometryFactory m_jtsFactory = new GeometryFactory();

  private HoverIndex m_hoverIndex = new HoverIndex();

  private final Map<EditInfo, Coordinate> m_paintOkPoints = new HashMap<>();

  private final List<Coordinate> m_paintCrossPoints = new ArrayList<>();

  private final List<Coordinate[]> m_paintOkLines = new ArrayList<>();

  private final List<Coordinate[]> m_paintCrossLines = new ArrayList<>();

  private final PolylineFigure m_okLineFigure;

  private final PointFigure m_okPointFigure;

  private final PolylineFigure m_crossLineFigure;

  private final PointFigure m_crossPointFigure;

  private final IObservation<TupleResult> m_observation;

  private final String m_domainComponentID;

  private final String m_valueComponentID;

  private final String m_classComponentId;

  private final ICoordinateMapper m_mapper;

  private final BuildingParameterLayer m_layer;

  private IDataRange<Double> m_domainRange;

  private IDataRange<Double> m_targetRange;

  private final IPointStyle m_okHoverStyle;

  private final IPointStyle m_crossHoverStyle;

  public BuildingParameterPaintData( final BuildingParameterLayer layer, final IStyleSet styleSet, final IObservation<TupleResult> observation, final String domainComponentID, final String valueComponentID, final String classComponentId, final ICoordinateMapper mapper )
  {
    m_layer = layer;
    m_observation = observation;
    m_domainComponentID = domainComponentID;
    m_valueComponentID = valueComponentID;
    m_classComponentId = classComponentId;
    m_mapper = mapper;

    m_okLineFigure = new PolylineFigure();
    m_okLineFigure.setStyle( styleSet.getStyle( "okLine", ILineStyle.class ) ); //$NON-NLS-1$

    m_okPointFigure = new PointFigure( styleSet.getStyle( "okPoint", IPointStyle.class ) ); //$NON-NLS-1$
    m_okHoverStyle = styleSet.getStyle( "okPointHover", IPointStyle.class ); //$NON-NLS-1$

    m_crossLineFigure = new PolylineFigure();
    m_crossLineFigure.setStyle( styleSet.getStyle( "crossLine", ILineStyle.class ) ); //$NON-NLS-1$

    m_crossPointFigure = new PointFigure( styleSet.getStyle( "crossPoint", IPointStyle.class ) ); //$NON-NLS-1$
    m_crossHoverStyle = styleSet.getStyle( "crossPointHover", IPointStyle.class ); //$NON-NLS-1$

    updatePaintData();
  }

  private void updatePaintData( )
  {
    final TupleResult result = m_observation.getResult();

    updateOkLines( result );
    updateIntersectionLines();
  }

  private void updateOkLines( final TupleResult result )
  {
    final int domainComponent = result.indexOfComponent( m_domainComponentID );
    final int valueComponent = result.indexOfComponent( m_valueComponentID );
    final int classComponent = result.indexOfComponent( m_classComponentId );

    Range<Double> domainRange = null;
    Range<Double> targetRange = null;

    // sort into coordinate arrays!
    BigDecimal lastClass = null;
    final List<Coordinate> crds = new ArrayList<>();

    for( final IRecord record : result )
    {
      final BigDecimal classValue = (BigDecimal)record.getValue( classComponent );
      final BigDecimal domainValue = (BigDecimal)record.getValue( domainComponent );
      final BigDecimal targetValue = (BigDecimal)record.getValue( valueComponent );

      if( domainValue == null || targetValue == null )
        continue;

      if( !ObjectUtils.equals( classValue, lastClass ) )
      {
        m_paintOkLines.add( crds.toArray( new Coordinate[crds.size()] ) );
        crds.clear();
      }

      // add value to path
      final Coordinate currentCrd = new Coordinate( domainValue.doubleValue(), targetValue.doubleValue() );

      final EditInfo info = buildOkPointInfo( record, domainValue, targetValue, classValue );

      m_paintOkPoints.put( info, currentCrd );
      crds.add( currentCrd );

      lastClass = classValue;

      /* update ranges */
      domainRange = updateRange( domainRange, domainValue );
      targetRange = updateRange( targetRange, targetValue );
    }

    // additionally add last line
    m_paintOkLines.add( crds.toArray( new Coordinate[crds.size()] ) );

    /* set ranges */
    if( domainRange != null )
      m_domainRange = new DataRange<>( domainRange.lowerEndpoint(), domainRange.upperEndpoint() );
    if( targetRange != null )
      m_targetRange = new DataRange<>( targetRange.lowerEndpoint(), targetRange.upperEndpoint() );
  }

  private Range<Double> updateRange( final Range<Double> range, final BigDecimal value )
  {
    if( value == null )
      return range;

    final Range<Double> valueRange = Ranges.singleton( value.doubleValue() );
    if( range == null )
      return valueRange;

    return range.span( valueRange );
  }

  private EditInfo buildOkPointInfo( final IRecord record, final BigDecimal domainValue, final BigDecimal targetValue, final BigDecimal classValue )
  {
    final TupleResult result = record.getOwner();

    final int domainComponentIndex = result.indexOfComponent( m_domainComponentID );
    final int valueComponentIndex = result.indexOfComponent( m_valueComponentID );
    final int classComponentIndex = result.indexOfComponent( m_classComponentId );

    final IComponent domainComponent = result.getComponent( domainComponentIndex );
    final IComponent valueComponent = result.getComponent( valueComponentIndex );
    final IComponent classComponent = result.getComponent( classComponentIndex );

    final String domainLabel = ComponentUtilities.getComponentName( domainComponent );
    final String valueLabel = ComponentUtilities.getComponentName( valueComponent );
    final String classLabel = ComponentUtilities.getComponentName( classComponent );

    final String domainUnit = ComponentUtilities.getComponentUnitLabel( domainComponent );
    final String valueUnit = ComponentUtilities.getComponentUnitLabel( valueComponent );
    final String classUnit = ComponentUtilities.getComponentUnitLabel( classComponent );

    // Edit info
    final String[] formats = new String[] { "%s", "%.3f", "%s" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    final int[] alignments = new int[] { SWT.LEFT, SWT.RIGHT, SWT.LEFT };
    final TooltipFormatter formatter = new TooltipFormatter( null, formats, alignments );

    formatter.addLine( classLabel, classValue, classUnit );
    formatter.addLine( domainLabel, domainValue, domainUnit );
    formatter.addLine( valueLabel, targetValue, valueUnit );

    final String msg = formatter.format();

    return new EditInfo( m_layer, null, null, record, msg, null );
  }

  private void updateIntersectionLines( )
  {
    // Determine intersections and intersecting lines
    // REMARK: brute force at the moment, we normally do not have too many lines. If performance problems occur, use
    // spatial-index for the lines
    final Set<Coordinate[]> crossLines = new HashSet<>();
    for( final Coordinate[] line1 : m_paintOkLines )
    {
      for( final Coordinate[] line2 : m_paintOkLines )
      {
        if( line1 == line2 || line1.length < 2 || line2.length < 2 )
          continue;

        final LineString ls1 = m_jtsFactory.createLineString( line1 );
        final LineString ls2 = m_jtsFactory.createLineString( line2 );
        if( ls1.intersects( ls2 ) )
        {
          crossLines.add( line1 );
          crossLines.add( line2 );

          final Geometry intersection = ls1.intersection( ls2 );
          m_paintCrossPoints.addAll( Arrays.asList( intersection.getCoordinates() ) );
        }
      }
    }

    m_paintOkLines.removeAll( crossLines );
    m_paintCrossLines.addAll( crossLines );
  }

  public void paint( final GC gc )
  {
    m_hoverIndex = new HoverIndex();

    for( final Coordinate[] okLine : m_paintOkLines )
    {
      m_okLineFigure.setPoints( toPointArray( Arrays.asList( okLine ) ) );
      m_okLineFigure.paint( gc );
    }

    for( final Coordinate[] crossLine : m_paintCrossLines )
    {
      m_crossLineFigure.setPoints( toPointArray( Arrays.asList( crossLine ) ) );
      m_crossLineFigure.paint( gc );
    }

    final Point[] okPoints = toPointArray( m_paintOkPoints.values() );
    m_okPointFigure.setCenterPoints( okPoints );
    m_okPointFigure.paint( gc );

    m_crossPointFigure.setCenterPoints( toPointArray( m_paintCrossPoints ) );
    m_crossPointFigure.paint( gc );

    /* update infos */
    updateOKEditInfos();
    updateCrossEditInfos();
  }

  private void updateOKEditInfos( )
  {
    for( final Entry<EditInfo, Coordinate> entry : m_paintOkPoints.entrySet() )
    {
      final EditInfo info = entry.getKey();
      final Coordinate crd = entry.getValue();

      final Point pos = m_mapper.logicalToScreen( crd.x, crd.y );
      final Rectangle shape = new Rectangle( pos.x - 5, pos.y - 5, 10, 10 );

      final PointFigure hoverPaintable = new PointFigure( m_okHoverStyle );
      hoverPaintable.setCenterPoint( pos );

      final EditInfo updatedInfo = new EditInfo( m_layer, hoverPaintable, null, info.getData(), info.getText(), null );

      // update edit infos
      m_hoverIndex.addElement( shape, updatedInfo );
    }
  }

  private void updateCrossEditInfos( )
  {
    for( final Coordinate crd : m_paintCrossPoints )
    {
      final Point pos = m_mapper.logicalToScreen( crd.x, crd.y );

      // Edit info
      final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.chart.BuildingParameterLayer.5" ); //$NON-NLS-1$
      final Rectangle shape = new Rectangle( pos.x - 5, pos.y - 5, 10, 10 );

      final PointFigure hoverPaintable = new PointFigure( m_crossHoverStyle );
      hoverPaintable.setCenterPoint( pos );

      final EditInfo info = new EditInfo( m_layer, hoverPaintable, null, null, msg, pos );
      m_hoverIndex.addElement( shape, info );
    }
  }

  private Point[] toPointArray( final Collection<Coordinate> crds )
  {
    final Collection<Point> points = new ArrayList<>( crds.size() );

    for( final Coordinate crd : crds )
      points.add( m_mapper.logicalToScreen( crd.x, crd.y ) );

    return points.toArray( new Point[points.size()] );
  }

  public IDataRange<Double> getDomainRange( )
  {
    return m_domainRange;
  }

  public IDataRange<Double> getTargetRange( )
  {
    return m_targetRange;
  }

  public IObservation<TupleResult> getObservation( )
  {
    return m_observation;
  }

  public EditInfo getInfo( final Point p )
  {
    if( m_hoverIndex == null )
      return null;

    return m_hoverIndex.findElement( p );
  }
}