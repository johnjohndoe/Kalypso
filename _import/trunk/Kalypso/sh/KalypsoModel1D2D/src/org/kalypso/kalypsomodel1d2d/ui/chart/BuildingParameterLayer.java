package org.kalypso.kalypsomodel1d2d.ui.chart;

import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang.ObjectUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.chart.ext.base.layer.AbstractChartLayer;
import org.kalypso.chart.framework.model.data.IDataContainer;
import org.kalypso.chart.framework.model.data.IDataRange;
import org.kalypso.chart.framework.model.data.impl.ComparableDataRange;
import org.kalypso.chart.framework.model.layer.EditInfo;
import org.kalypso.chart.framework.model.layer.IEditableChartLayer;
import org.kalypso.chart.framework.model.mapper.IAxis;
import org.kalypso.chart.framework.model.styles.IStyledElement;
import org.kalypso.chart.framework.model.styles.IStyleConstants.SE_TYPE;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

public class BuildingParameterLayer extends AbstractChartLayer<BigDecimal, BigDecimal> implements IDataContainer<BigDecimal, BigDecimal>, IEditableChartLayer<BigDecimal, BigDecimal>
{
  private final static GeometryFactory GF = new GeometryFactory();

  private Coordinate[] m_paintOkPoints = null;

  private Coordinate[] m_paintCrossPoints = null;

  private final List<Coordinate[]> m_paintOkLines = new ArrayList<Coordinate[]>();

  private final List<Coordinate[]> m_paintCrossLines = new ArrayList<Coordinate[]>();

  private final int m_domainComponent;

  private final int m_valueComponent;

  private final int m_classComponent;

  private final ToolTipRenderer m_tooltipRenderer = new ToolTipRenderer();

  private Point m_tooltipPoint;

  private EditInfo[] m_editInfos;

  private final Feature m_obsFeature;

  private final TupleResult m_result;

  public BuildingParameterLayer( final IAxis<BigDecimal> domainAxis, final IAxis<BigDecimal> targetAxis, final Feature obsFeature, final String domainComponentId, final String valueComponentId, final String classComponentId )
  {
    super( domainAxis, targetAxis );

    final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( obsFeature );
    final TupleResult result = obs.getResult();

    m_domainComponent = result.indexOfComponent( domainComponentId );
    m_valueComponent = result.indexOfComponent( valueComponentId );
    m_classComponent = result.indexOfComponent( classComponentId );

    // REMARK: at the moment, we do not react to changes in the result; if we do later, this must be done every time the
    // result is changed
    m_obsFeature = obsFeature;
    m_result = result;

    setDataContainer( this );

    updatePaintData();
  }

  public void drawIcon( final Image img )
  {
    // we do not have an legend item (yet?)
  }

  public void paint( final GCWrapper gc )
  {
    final IStyledElement okLineStyle = getStyle().getElement( SE_TYPE.LINE, 0 );
    final IStyledElement okPointStyle = getStyle().getElement( SE_TYPE.POINT, 0 );
    final IStyledElement crossLineStyle = getStyle().getElement( SE_TYPE.LINE, 1 );
    final IStyledElement crossPointStyle = getStyle().getElement( SE_TYPE.POINT, 1 );

    for( final Coordinate[] okLine : m_paintOkLines )
    {
      okLineStyle.setPath( toPointList( okLine ) );
      okLineStyle.paint( gc );
    }

    for( final Coordinate[] crossLine : m_paintCrossLines )
    {
      crossLineStyle.setPath( toPointList( crossLine ) );
      crossLineStyle.paint( gc );
    }

    final List<Point> okPoints = toPointList( m_paintOkPoints );
    okPointStyle.setPath( okPoints );
    okPointStyle.paint( gc );

    crossPointStyle.setPath( toPointList( m_paintCrossPoints ) );
    crossPointStyle.paint( gc );

    m_editInfos = updateEditInfos();

    if( m_tooltipPoint != null )
      m_tooltipRenderer.paintTooltip( m_tooltipPoint, gc.m_gc, gc.getClipping() );
  }

  private EditInfo[] updateEditInfos( )
  {
    final String classLabel = m_result.getComponent( m_classComponent ).getName();

    // Create current edit infos from ok points
    final List<EditInfo> editInfos = new ArrayList<EditInfo>( m_result.size() );
    final IAxis<BigDecimal> xAxis = getDomainAxis();
    final IAxis<BigDecimal> yAxis = getTargetAxis();
    for( final IRecord record : m_result )
    {
      final BigDecimal classValue = (BigDecimal) record.getValue( m_classComponent );
      final BigDecimal domainValue = (BigDecimal) record.getValue( m_domainComponent );
      final BigDecimal targetValue = (BigDecimal) record.getValue( m_valueComponent );

      // convert to screen-point
      final int x = xAxis.logicalToScreen( domainValue );
      final int y = yAxis.logicalToScreen( targetValue );
      final Point pos = new Point( x, y );

      // Edit info
      final String msg = String.format( "%10.4f\t%s%n%10.4f\t%s%n%10.4f\t%s", domainValue, xAxis.getLabel(), targetValue, yAxis.getLabel(), classValue, classLabel );
      final Rectangle shape = new Rectangle( x - 5, y - 5, 10, 10 );
      final EditInfo info = new EditInfo( null, shape, record, msg, pos );
      editInfos.add( info );
    }

    for( final Coordinate crd : m_paintCrossPoints )
    {
      // convert to screen-point
      final int x = xAxis.logicalToScreen( new BigDecimal( crd.x ) );
      final int y = yAxis.logicalToScreen( new BigDecimal( crd.y ) );
      final Point pos = new Point( x, y );

      // Edit info
      final String msg = String.format( "Schnittpunkt" );
      final Rectangle shape = new Rectangle( x - 5, y - 5, 10, 10 );
      final EditInfo info = new EditInfo( null, shape, null, msg, pos );
      editInfos.add( info );
    }

    return editInfos.toArray( new EditInfo[editInfos.size()] );
  }

  private List<Point> toPointList( final Coordinate[] crds )
  {
    final IAxis<BigDecimal> xAxis = getDomainAxis();
    final IAxis<BigDecimal> yAxis = getTargetAxis();

    final List<Point> points = new ArrayList<Point>( crds.length );

    for( final Coordinate crd : crds )
    {
      final int x = xAxis.logicalToScreen( new BigDecimal( crd.x ) );
      final int y = yAxis.logicalToScreen( new BigDecimal( crd.y ) );
      final Point pos = new Point( x, y );
      points.add( pos );
    }

    return points;
  }

  /**
   * @see org.kalypso.chart.framework.model.data.IDataContainer#close()
   */
  public void close( )
  {
  }

  /**
   * @see org.kalypso.chart.framework.model.data.IDataContainer#isOpen()
   */
  public boolean isOpen( )
  {
    return true;
  }

  /**
   * @see org.kalypso.chart.framework.model.data.IDataContainer#open()
   */
  public void open( )
  {
  }

  /**
   * @see org.kalypso.chart.framework.model.data.IDataContainer#getDomainRange()
   */
  public IDataRange<BigDecimal> getDomainRange( )
  {
    return rangeForComponent( m_domainComponent );
  }

  /**
   * @see org.kalypso.chart.framework.model.data.IDataContainer#getTargetRange()
   */
  public IDataRange<BigDecimal> getTargetRange( )
  {
    return rangeForComponent( m_valueComponent );
  }

  private IDataRange<BigDecimal> rangeForComponent( final int component )
  {
    BigDecimal min = new BigDecimal( Double.MAX_VALUE );
    BigDecimal max = new BigDecimal( Double.MIN_VALUE );

    for( final IRecord record : m_result )
    {
      final BigDecimal value = (BigDecimal) record.getValue( component );
      max = max.max( value );
      min = min.min( value );
    }

    return new ComparableDataRange<BigDecimal>( new BigDecimal[] { min, max } );
  }

  public EditInfo getEditInfo( final Point p )
  {
    if( m_editInfos == null )
      return null;

    final EditInfo[] infos = m_editInfos;
    for( final EditInfo editInfo : infos )
    {
      if( editInfo.shape.contains( p ) )
        return editInfo;
    }

    return null;
  }

  public void setTooltip( final String text, final Point point )
  {
    m_tooltipPoint = point;
    m_tooltipRenderer.setTooltip( text );

    getEventHandler().fireLayerContentChanged( this );
  }

  public void delete( final EditInfo info )
  {
    if( info == null )
      return;

    final IRecord record = (IRecord) info.data;
    m_result.remove( record );

    updatePaintData();
    getEventHandler().fireLayerContentChanged( this );
  }

  private void updatePaintData( )
  {
    m_paintCrossLines.clear();
    m_paintOkLines.clear();
    m_paintCrossPoints = null;
    m_paintOkPoints = null;

    // sort into coordinate arrays!
    BigDecimal lastClass = null;
    final List<Coordinate> crds = new ArrayList<Coordinate>();

    final List<Coordinate> okPoints = new ArrayList<Coordinate>( m_result.size() );
    for( final IRecord record : m_result )
    {
      final BigDecimal classValue = (BigDecimal) record.getValue( m_classComponent );
      final BigDecimal domainValue = (BigDecimal) record.getValue( m_domainComponent );
      final BigDecimal targetValue = (BigDecimal) record.getValue( m_valueComponent );

      if( !ObjectUtils.equals( classValue, lastClass ) )
      {
        m_paintOkLines.add( crds.toArray( new Coordinate[crds.size()] ) );
        crds.clear();
      }

      // add value to path
      final Coordinate currentCrd = new Coordinate( domainValue.doubleValue(), targetValue.doubleValue() );

      okPoints.add( currentCrd );
      crds.add( currentCrd );

      lastClass = classValue;
    }

    // additionally add last line
    m_paintOkLines.add( crds.toArray( new Coordinate[crds.size()] ) );
    crds.clear();

    m_paintOkPoints = okPoints.toArray( new Coordinate[okPoints.size()] );

    // Determine intersections and intersecting lines
    // REMARK: brute force at the moment, we normally do not have too many lines. If performance problems occur, use
    // spatial-index for the lines
    final Set<Coordinate[]> crossLines = new HashSet<Coordinate[]>();
    final Set<Coordinate> crossPoints = new HashSet<Coordinate>();
    for( final Coordinate[] line1 : m_paintOkLines )
    {
      for( final Coordinate[] line2 : m_paintOkLines )
      {
        if( line1 == line2 || line1.length < 2 || line2.length < 2 )
          continue;

        final LineString ls1 = GF.createLineString( line1 );
        final LineString ls2 = GF.createLineString( line2 );
        if( ls1.intersects( ls2 ) )
        {
          crossLines.add( line1 );
          crossLines.add( line2 );

          final Geometry intersection = ls1.intersection( ls2 );
          crossPoints.addAll( Arrays.asList( intersection.getCoordinates() ) );
        }
      }
    }

    m_paintOkLines.removeAll( crossLines );
    m_paintCrossLines.addAll( crossLines );
    m_paintCrossPoints = crossPoints.toArray( new Coordinate[crossPoints.size()] );
  }

  /**
   * Writes the cloned result back into the real result.
   */
  public void saveData( final IProgressMonitor monitor ) throws InvocationTargetException, CoreException
  {
    monitor.beginTask( "Speichere Parameter", 3 );

    final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( m_obsFeature );
    final TupleResult result = obs.getResult();
    result.clear();

    ProgressUtilities.worked( monitor, 1 );

    final int compCount = result.getComponents().length;
    for( final IRecord record : m_result )
    {
      final IRecord newRecord = result.createRecord();

      for( int i = 0; i < compCount; i++ )
        newRecord.setValue( i, record.getValue( i ) );

      result.add( newRecord );
    }

    ProgressUtilities.worked( monitor, 1 );

    final FeatureChange[] changes = ObservationFeatureFactory.toFeatureAsChanges( obs, m_obsFeature );
    final ChangeFeaturesCommand command = new ChangeFeaturesCommand( m_obsFeature.getWorkspace(), changes );

    // HACK: we post the command directly to the flow-model.... it would be of course much better to get the commandable
    // workspace from outside...

    KalypsoAFGUIFrameworkPlugin.getDefault().getDataProvider().postCommand( IFlowRelationshipModel.class, command );

    ProgressUtilities.worked( monitor, 1 );

    ProgressUtilities.done( monitor );
  }

  /**
   * @see org.kalypso.chart.framework.model.layer.IEditableChartLayer#alwaysAllowsEditing()
   */
  public boolean alwaysAllowsEditing( )
  {
    return true;
  }

  /**
   * @see org.kalypso.chart.framework.model.layer.IEditableChartLayer#edit(org.eclipse.swt.graphics.Point,
   *      org.kalypso.chart.framework.model.layer.EditInfo)
   */
  public EditInfo edit( final Point point, final EditInfo info )
  {
    // find real point from point
    final IAxis<BigDecimal> xAxis = getDomainAxis();
    final IAxis<BigDecimal> yAxis = getTargetAxis();

    final BigDecimal xValue = new BigDecimal( ((Number) xAxis.screenToLogical( point.x )).doubleValue() );
    final BigDecimal yValue = new BigDecimal( ((Number) yAxis.screenToLogical( point.y )).doubleValue() );

    final IRecord record = (IRecord) info.data;

    record.setValue( m_domainComponent, xValue );
    record.setValue( m_valueComponent, yValue );

    updatePaintData();
    getEventHandler().fireLayerContentChanged( this );

    return info;
  }

  /**
   * @see org.kalypso.chart.framework.model.layer.IEditableChartLayer#setActivePoint(java.lang.Object)
   */
  public void setActivePoint( final Object data )
  {
    throw new UnsupportedOperationException();
  }

}
