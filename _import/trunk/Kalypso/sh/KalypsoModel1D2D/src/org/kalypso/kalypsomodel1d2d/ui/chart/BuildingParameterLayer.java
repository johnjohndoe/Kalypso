package org.kalypso.kalypsomodel1d2d.ui.chart;

import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.ObjectUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.graphics.GC;
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

public class BuildingParameterLayer extends AbstractChartLayer<BigDecimal, BigDecimal> implements IDataContainer<BigDecimal, BigDecimal>, IEditableChartLayer<BigDecimal, BigDecimal>
{
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
  }

  public void drawIcon( final Image img )
  {
    // final Rectangle bounds = img.getBounds();
    // final int height = bounds.height;
    // final int width = bounds.width;
    final GC gc = new GC( img );
    //
    // final ArrayList<Point> path = new ArrayList<Point>();
    //
    // path.add( new Point( 0, height / 2 ) );
    // path.add( new Point( width / 5, height / 2 ) );
    // path.add( new Point( width / 5 * 2, height / 4 ) );
    // path.add( new Point( width / 5 * 3, height / 4 * 3 ) );
    // path.add( new Point( width / 5 * 4, height / 2 ) );
    // path.add( new Point( width, height / 2 ) );
    //
    // final IStyledElement element = getStyle().getElement( SE_TYPE.LINE, 1 );
    //
    // element.setPath( path );
    // element.paint( gcw );
    //

    gc.dispose();
  }

  public void paint( final GCWrapper gc )
  {
    final IAxis<BigDecimal> xAxis = getDomainAxis();
    final IAxis<BigDecimal> yAxis = getTargetAxis();

    final IStyledElement sl = getStyle().getElement( SE_TYPE.LINE, 0 );
    final IStyledElement sp = getStyle().getElement( SE_TYPE.POINT, 0 );

    BigDecimal lastClass = null;
    final List<Point> path = new ArrayList<Point>();
    final List<EditInfo> editInfos = new ArrayList<EditInfo>();
    for( final IRecord record : m_result )
    {
      final BigDecimal classValue = (BigDecimal) record.getValue( m_classComponent );
      final BigDecimal domainValue = (BigDecimal) record.getValue( m_domainComponent );
      final BigDecimal targetValue = (BigDecimal) record.getValue( m_valueComponent );

      if( !ObjectUtils.equals( classValue, lastClass ) )
      {
        drawPath( gc, sl, sp, path );

        // clear current path
        path.clear();
      }

      // add value to path
      final int x = xAxis.logicalToScreen( domainValue );
      final int y = yAxis.logicalToScreen( targetValue );
      final Point pos = new Point( x, y );
      path.add( pos );

      // Edit info
      final String msg = String.format( "%.4f / %.4f (%.4f)", domainValue, targetValue, classValue );
      final Rectangle shape = new Rectangle( x - 5, y - 5, 10, 10 );
      final EditInfo info = new EditInfo( null, shape, record, msg, pos );
      editInfos.add( info );

      lastClass = classValue;
    }

    drawPath( gc, sl, sp, path );

    m_editInfos = editInfos.toArray( new EditInfo[editInfos.size()] );

    if( m_tooltipPoint != null )
      m_tooltipRenderer.paintTooltip( m_tooltipPoint, gc.m_gc, gc.getClipping() );
  }

  private void drawPath( final GCWrapper gc, final IStyledElement sl, final IStyledElement sp, final List<Point> path )
  {
    if( path.isEmpty() )
      return;

    // draw old path
    sl.setPath( path );
    sl.paint( gc );
    sp.setPath( path );
    sp.paint( gc );
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

    // TODO
    // - redraw via tuple result mechanism

    getEventHandler().fireLayerContentChanged( this );
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

    // TODO
    // - redraw via tuple result mechanism
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
