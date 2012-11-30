package org.kalypso.kalypsomodel1d2d.ui.chart;

import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;

import de.openali.odysseus.chart.factory.layer.AbstractChartLayer;
import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.event.ILayerManagerEventListener.ContentChangeType;
import de.openali.odysseus.chart.framework.model.figure.impl.PointFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IStyleConstants.LINECAP;
import de.openali.odysseus.chart.framework.model.style.IStyleConstants.LINEJOIN;
import de.openali.odysseus.chart.framework.model.style.IStyleSet;
import de.openali.odysseus.chart.framework.model.style.impl.LineStyle;
import de.openali.odysseus.chart.framework.model.style.impl.StyleSet;
import de.openali.odysseus.chart.framework.util.img.ChartImageInfo;
import de.openali.odysseus.chart.framework.util.resource.IPair;

public class BuildingParameterLayer extends AbstractChartLayer implements IEditableChartLayer
{
  private final ModellEventListener m_modelListener = new ModellEventListener()
  {
    @Override
    public void onModellChange( final ModellEvent modellEvent )
    {
      handleFeatureChanged( modellEvent );
    }
  };

  private final String m_domainComponentID;

  private final String m_valueComponentID;

  private final String m_classComponentId;

  private final Feature m_obsFeature;

  private final IStyleSet m_styleSet;

  private BuildingParameterPaintData m_paintData;

  public BuildingParameterLayer( final Feature obsFeature, final String domainComponentId, final String valueComponentId, final String classComponentId, final IStyleSet styleSet )
  {
    super( null, new StyleSet() );

    // REMARK: at the moment, we do not react to changes in the result; if we do later, this must be done every time the
    // result is changed
    m_obsFeature = obsFeature;
    m_domainComponentID = domainComponentId;
    m_valueComponentID = valueComponentId;
    m_classComponentId = classComponentId;

    m_styleSet = styleSet;

    m_obsFeature.getWorkspace().addModellListener( m_modelListener );

    updatePaintData();
  }

  @Override
  public final void setCoordinateMapper( final ICoordinateMapper coordinateMapper )
  {
    super.setCoordinateMapper( coordinateMapper );

    updatePaintData();
  }

  @Override
  public void paint( final GC gc, final ChartImageInfo chartImageInfo, final IProgressMonitor monitor )
  {
    if( m_paintData == null )
      return;

    m_paintData.paint( gc );
  }

  @Override
  public IDataRange<Double> getDomainRange( )
  {
    if( m_paintData == null )
      return null;

    return m_paintData.getDomainRange();
  }

  @Override
  public IDataRange<Double> getTargetRange( final IDataRange< ? > domainIntervall )
  {
    if( m_paintData == null )
      return null;

    return m_paintData.getTargetRange();
  }

  public EditInfo getEditInfo( final Point p )
  {
    if( m_paintData == null )
      return null;

    return m_paintData.getInfo( p );
  }

  public void delete( final EditInfo info )
  {
    if( info == null )
      return;

    if( m_paintData == null )
      return;

    final IObservation<TupleResult> obs = m_paintData.getObservation();

    final IRecord record = (IRecord)info.getData();
    obs.getResult().remove( record );

    saveData();
  }

  private void updatePaintData( )
  {
    final ICoordinateMapper mapper = getCoordinateMapper();
    if( mapper != null )
    {
      final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( m_obsFeature );

      m_paintData = new BuildingParameterPaintData( this, m_styleSet, obs, m_domainComponentID, m_valueComponentID, m_classComponentId, mapper );
    }

    getEventHandler().fireLayerContentChanged( this, ContentChangeType.value );
  }

  /**
   * Writes the cloned result back into the real result.
   */
  private void saveData( )
  {
    if( m_paintData == null )
      return;

    final IObservation<TupleResult> observation = m_paintData.getObservation();

    final FeatureChange[] changes = ObservationFeatureFactory.toFeatureAsChanges( observation, m_obsFeature );
    final ChangeFeaturesCommand command = new ChangeFeaturesCommand( m_obsFeature.getWorkspace(), changes );

    try
    {
      // HACK: we post the command directly to the flow-model.... it would be of course much better to get the commandable
      // workspace from outside...
      KalypsoAFGUIFrameworkPlugin.getDataProvider().postCommand( IFlowRelationshipModel.class.getName(), command );
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();
    }
  }

  @Override
  public void dispose( )
  {
    m_obsFeature.getWorkspace().removeModellListener( m_modelListener );
  }

  @Override
  public EditInfo commitDrag( final Point point, final EditInfo dragStartData )
  {
    final IPair<Object, Object> logical = getCoordinateMapper().screenToLogical( point );

    final IRecord record = (IRecord)dragStartData.getData();
    if( record == null )
      return null;

    final TupleResult result = record.getOwner();
    final int domainComponent = result.indexOfComponent( m_domainComponentID );
    final int valueComponent = result.indexOfComponent( m_valueComponentID );

    final Number domain = (Number)logical.getDomain();
    final Number target = (Number)logical.getTarget();

    record.setValue( domainComponent, new BigDecimal( domain.toString() ) );
    record.setValue( valueComponent, new BigDecimal( target.toString() ) );

    saveData();

    return null;
  }

  @Override
  public EditInfo drag( final Point newPos, final EditInfo dragStartData )
  {
    final Object data = dragStartData.getData();

    /* Only allow a drag, if something is selected. */
    if( !(data instanceof IRecord) )
    {
      final String msg = "Cannot drag crossing, move data point instead.";
      return new EditInfo( this, dragStartData.getHoverFigure(), null, null, msg, newPos );
    }

    final IRecord record = (IRecord)data;

    /* calculate screen pos of start element */
    final TupleResult result = record.getOwner();
    final int domainComponent = result.indexOfComponent( m_domainComponentID );
    final int valueComponent = result.indexOfComponent( m_valueComponentID );
    final BigDecimal domainValue = (BigDecimal)record.getValue( domainComponent );
    final BigDecimal targetValue = (BigDecimal)record.getValue( valueComponent );

    /* Get the target axis. */
    final Point startPos = getCoordinateMapper().logicalToScreen( domainValue, targetValue );

    /* Create a polyline figure. */
    final ILineStyle dragStyle = new LineStyle( 2, new RGB( 0, 0, 0 ), 255, 0f, new float[] { 10f, 10f }, LINEJOIN.ROUND, LINECAP.ROUND, 0, true );

    final PolylineFigure pf = new PolylineFigure( dragStyle );
    pf.setPoints( new Point[] { startPos, newPos } );

    final PointFigure hoverFigure = (PointFigure)dragStartData.getHoverFigure();
    final PointFigure draggedFigure = new PointFigure( hoverFigure.getStyle() );
    draggedFigure.setCenterPoint( newPos );

    /* clear tooltip when dragging */
    final String text = null;

    return new EditInfo( this, draggedFigure, pf, dragStartData.getData(), text, newPos );
  }

  @Override
  public EditInfo getHover( final Point pos )
  {
    return getEditInfo( pos );
  }

  private void handleObservationChanged( )
  {
    updatePaintData();

    getEventHandler().fireLayerContentChanged( this, ContentChangeType.value );
  }

  protected void handleFeatureChanged( final ModellEvent modellEvent )
  {
    if( modellEvent instanceof FeaturesChangedModellEvent )
    {
      final FeaturesChangedModellEvent featuresChangedEvent = (FeaturesChangedModellEvent)modellEvent;
      final Feature[] changedFeatures = featuresChangedEvent.getFeatures();
      if( ArrayUtils.contains( changedFeatures, m_obsFeature ) )
        updatePaintData();
    }
  }
}