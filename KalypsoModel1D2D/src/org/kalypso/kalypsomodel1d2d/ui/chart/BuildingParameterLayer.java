package org.kalypso.kalypsomodel1d2d.ui.chart;

import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.ObjectUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
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

import de.openali.odysseus.chart.factory.layer.AbstractChartLayer;
import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.data.impl.ComparableDataRange;
import de.openali.odysseus.chart.framework.model.figure.IPaintable;
import de.openali.odysseus.chart.framework.model.figure.impl.EmptyRectangleFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.PointFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;
import de.openali.odysseus.chart.framework.model.mapper.IAxis;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;
import de.openali.odysseus.chart.framework.model.style.IStyleSet;
import de.openali.odysseus.chart.framework.util.StyleUtils;

public class BuildingParameterLayer extends AbstractChartLayer implements IEditableChartLayer
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

  private Map<Rectangle, EditInfo> m_editInfos;

  private final Feature m_obsFeature;

  private final TupleResult m_result;

  private final IStyleSet m_styleSet;

  private final PolylineFigure m_okLineFigure;

  private final PointFigure m_okPointFigure;

  private final PolylineFigure m_crossLineFigure;

  private final PointFigure m_crossPointFigure;

  public BuildingParameterLayer( final Feature obsFeature, final String domainComponentId, final String valueComponentId, final String classComponentId, final IStyleSet styleSet )
  {
    super( null );

    final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( obsFeature );
    final TupleResult result = obs.getResult();

    m_domainComponent = result.indexOfComponent( domainComponentId );
    m_valueComponent = result.indexOfComponent( valueComponentId );
    m_classComponent = result.indexOfComponent( classComponentId );

    // REMARK: at the moment, we do not react to changes in the result; if we do later, this must be done every time the
    // result is changed
    m_obsFeature = obsFeature;
    m_result = result;

    m_styleSet = styleSet;
    m_okLineFigure = new PolylineFigure();
    m_okLineFigure.setStyle( m_styleSet.getStyle( "okLine", ILineStyle.class ) ); //$NON-NLS-1$
    m_okPointFigure = new PointFigure();
    m_okPointFigure.setStyle( m_styleSet.getStyle( "okPoint", IPointStyle.class ) ); //$NON-NLS-1$
    m_crossLineFigure = new PolylineFigure();
    m_crossLineFigure.setStyle( m_styleSet.getStyle( "crossLine", ILineStyle.class ) ); //$NON-NLS-1$
    m_crossPointFigure = new PointFigure();
    m_crossPointFigure.setStyle( m_styleSet.getStyle( "crossPoint", IPointStyle.class ) ); //$NON-NLS-1$

    updatePaintData();
  }

  public void drawIcon( final Image img )
  {
    // we do not have an legend item (yet?)
  }

  @Override
  public void paint( final GC gc )
  {

    for( final Coordinate[] okLine : m_paintOkLines )
    {
      m_okLineFigure.setPoints( toPointArray( okLine ) );
      m_okLineFigure.paint( gc );
    }

    for( final Coordinate[] crossLine : m_paintCrossLines )
    {
      m_crossLineFigure.setPoints( toPointArray( crossLine ) );
      m_crossLineFigure.paint( gc );
    }

    final Point[] okPoints = toPointArray( m_paintOkPoints );
    m_okPointFigure.setPoints( okPoints );
    m_okPointFigure.paint( gc );

    m_crossPointFigure.setPoints( toPointArray( m_paintCrossPoints ) );
    m_crossPointFigure.paint( gc );

    m_editInfos = updateEditInfos();

    if( m_tooltipPoint != null )
      m_tooltipRenderer.paintTooltip( m_tooltipPoint, gc, gc.getClipping() );
  }

  private Map<Rectangle, EditInfo> updateEditInfos( )
  {
    final String classLabel = m_result.getComponent( m_classComponent ).getName();

    // Create current edit infos from ok points
    final Map<Rectangle, EditInfo> editInfos = new LinkedHashMap<Rectangle, EditInfo>( m_result.size() );
    final IAxis xAxis = getDomainAxis();
    final IAxis yAxis = getTargetAxis();
    for( final IRecord record : m_result )
    {
      final BigDecimal classValue = (BigDecimal) record.getValue( m_classComponent );
      final BigDecimal domainValue = (BigDecimal) record.getValue( m_domainComponent );
      final BigDecimal targetValue = (BigDecimal) record.getValue( m_valueComponent );

      // convert to screen-point
      final int x = xAxis.numericToScreen( domainValue );
      final int y = yAxis.numericToScreen( targetValue );
      final Point pos = new Point( x, y );

      // Edit info
      final String msg = String.format( "%10.4f\t%s%n%10.4f\t%s%n%10.4f\t%s", domainValue, xAxis.getLabel(), targetValue, yAxis.getLabel(), classValue, classLabel ); //$NON-NLS-1$
      final Rectangle shape = new Rectangle( x - 5, y - 5, 10, 10 );
      final EditInfo info = new EditInfo( null, createHoverPaintable( shape ), null, record, msg, pos );
      editInfos.put( shape, info );
    }

    for( final Coordinate crd : m_paintCrossPoints )
    {
      // convert to screen-point
      final int x = xAxis.numericToScreen( new BigDecimal( crd.x ) );
      final int y = yAxis.numericToScreen( new BigDecimal( crd.y ) );
      final Point pos = new Point( x, y );

      // Edit info
      final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.chart.BuildingParameterLayer.5" ); //$NON-NLS-1$
      final Rectangle shape = new Rectangle( x - 5, y - 5, 10, 10 );
      final EditInfo info = new EditInfo( null, createHoverPaintable( shape ), null, null, msg, pos );
      editInfos.put( shape, info );
    }

    return editInfos;
  }

  private IPaintable createHoverPaintable( final Rectangle shape )
  {
    final ILineStyle dls = StyleUtils.getDefaultLineStyle();
    final EmptyRectangleFigure r = new EmptyRectangleFigure();
    r.setStyle( dls );
    r.setRectangle( shape );
    return r;
  }

  private Point[] toPointArray( final Coordinate[] crds )
  {
    final IAxis xAxis = getDomainAxis();
    final IAxis yAxis = getTargetAxis();

    final Point[] points = new Point[crds.length];
    for( int i = 0; i < crds.length; i++ )
    {
      final Coordinate crd = crds[i];
      final int x = xAxis.numericToScreen( new BigDecimal( crd.x ) );
      final int y = yAxis.numericToScreen( new BigDecimal( crd.y ) );
      final Point pos = new Point( x, y );
      points[i] = pos;
    }
    return points;
  }

  /**
   * @see org.kalypso.chart.framework.model.data.IDataContainer#getDomainRange()
   */
  @Override
  public IDataRange<Number> getDomainRange( )
  {
    return rangeForComponent( m_domainComponent );
  }

  /**
   * @see org.kalypso.chart.framework.model.data.IDataContainer#getTargetRange()
   */
  @Override
  public IDataRange<Number> getTargetRange( final IDataRange<Number> domainIntervall )
  {
    return rangeForComponent( m_valueComponent );
  }

  private IDataRange<Number> rangeForComponent( final int component )
  {
    BigDecimal min = new BigDecimal( Double.MAX_VALUE );
    BigDecimal max = new BigDecimal( -Double.MAX_VALUE );

    for( final IRecord record : m_result )
    {
      final BigDecimal value = (BigDecimal) record.getValue( component );
      max = max.max( value );
      min = min.min( value );
    }

    return new ComparableDataRange<Number>( new BigDecimal[] { min, max } );
  }

  public EditInfo getEditInfo( final Point p )
  {
    if( m_editInfos == null )
      return null;

    for( final Rectangle shape : m_editInfos.keySet() )
    {
      if( shape.contains( p ) )
        return m_editInfos.get( shape );
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

    final IRecord record = (IRecord) info.getData();
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
    monitor.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.chart.BuildingParameterLayer.6" ), 3 ); //$NON-NLS-1$

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
    final IAxis xAxis = getDomainAxis();
    final IAxis yAxis = getTargetAxis();

    final BigDecimal xValue = new BigDecimal( (xAxis.screenToNumeric( point.x )).doubleValue() );
    final BigDecimal yValue = new BigDecimal( (yAxis.screenToNumeric( point.y )).doubleValue() );

    final IRecord record = (IRecord) info.getData();

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

  /**
   * @see de.openali.odysseus.chart.ext.base.layer.AbstractChartLayer#createLegendEntries()
   */
  @Override
  protected ILegendEntry[] createLegendEntries( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#dispose()
   */
  @Override
  public void dispose( )
  {
    // nothing to do
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer#commitDrag(org.eclipse.swt.graphics.Point,
   *      de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */
  @Override
  public EditInfo commitDrag( final Point point, final EditInfo dragStartData )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer#drag(org.eclipse.swt.graphics.Point,
   *      de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */
  @Override
  public EditInfo drag( final Point newPos, final EditInfo dragStartData )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer#getHover(org.eclipse.swt.graphics.Point)
   */
  @Override
  public EditInfo getHover( final Point pos )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer#isLocked()
   */
  @Override
  public boolean isLocked( )
  {
    // TODO Auto-generated method stub
    return false;
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer#lockLayer(boolean)
   */
  @Override
  public void lockLayer( final boolean isLocked )
  {
    // TODO Auto-generated method stub

  }

}
