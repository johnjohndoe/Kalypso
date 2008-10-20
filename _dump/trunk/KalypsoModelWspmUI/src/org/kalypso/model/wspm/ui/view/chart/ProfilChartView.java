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
package org.kalypso.model.wspm.ui.view.chart;

import java.util.SortedMap;
import java.util.TreeMap;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPersistableElement;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.chart.ui.IChartPart;
import org.kalypso.chart.ui.editor.commandhandler.ChartHandlerUtilities;
import org.kalypso.chart.ui.editor.mousehandler.AxisDragHandlerDelegate;
import org.kalypso.chart.ui.editor.mousehandler.PlotDragHandlerDelegate;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.result.IStationResult;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIExtensions;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.model.wspm.ui.view.chart.handler.ClickHandler;

import de.openali.odysseus.chart.ext.base.axis.GenericLinearAxis;
import de.openali.odysseus.chart.ext.base.axisrenderer.AxisRendererConfig;
import de.openali.odysseus.chart.ext.base.axisrenderer.GenericAxisRenderer;
import de.openali.odysseus.chart.ext.base.axisrenderer.GenericNumberTickCalculator;
import de.openali.odysseus.chart.ext.base.axisrenderer.NumberLabelCreator;
import de.openali.odysseus.chart.framework.model.impl.ChartModel;
import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.IExpandableChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILayerManager;
import de.openali.odysseus.chart.framework.model.mapper.IAxis;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.mapper.IAxisConstants.POSITION;
import de.openali.odysseus.chart.framework.model.mapper.impl.AxisAdjustment;
import de.openali.odysseus.chart.framework.model.mapper.registry.IMapperRegistry;
import de.openali.odysseus.chart.framework.model.mapper.renderer.IAxisRenderer;
import de.openali.odysseus.chart.framework.util.ChartUtilities;
import de.openali.odysseus.chart.framework.view.impl.ChartComposite;

/**
 * @author belger
 * @author kimwerner
 */
public class ProfilChartView extends AbstractProfilView implements IPersistableElement
{

  private static final String MEM_LAYER_VIS = "layerVisibility"; //$NON-NLS-1$

  private static final String MEM_LAYER_POS = "layerPosition"; //$NON-NLS-1$

  private static final String MEM_LAYER_DAT = "layerData"; //$NON-NLS-1$

  private static final String MEM_LAYER_CLD = "layerChilds"; //$NON-NLS-1$

  private static final String MEM_LAYER_ACT = "activeLayer"; //$NON-NLS-1$

  public static final String ID_AXIS_DOMAIN = "domain";//$NON-NLS-1$

  public static final String ID_AXIS_LEFT = "left";//$NON-NLS-1$

  public static final String ID_AXIS_RIGHT = "right";//$NON-NLS-1$

  public static final int AXIS_GAP = 5; // distance between layers and Axis

  private ICoordinateMapper m_mapper;

  private ChartComposite m_chart = null;

  private IProfilLayerProvider m_layerProvider;

  private PlotDragHandlerDelegate m_plotDragHandler;

  private AxisDragHandlerDelegate m_axisDragHandler;

  private IMemento m_memento = null;

  public ProfilChartView( final IProfil profile )
  {
    this( profile, null );
  }

  public ProfilChartView( final IProfil profile, final IStationResult[] results )// , final ColorRegistry colorRegistry
  {
    super( profile, results );
  }

  @SuppressWarnings("boxing")//$NON-NLS-1$
  protected void createLayer( )
  {

    final ILayerManager lm = m_chart.getChartModel().getLayerManager();
    if( m_memento != null )
      saveState( m_memento, lm );

    for( final IChartLayer layer : lm.getLayers() )
      lm.removeLayer( layer );

    final IProfil profil = getProfil();
    if( profil != null )
    {
      if( m_layerProvider == null )
      {
        m_layerProvider = KalypsoModelWspmUIExtensions.createProfilLayerProvider( profil.getType() );

        if( m_layerProvider == null )
          // TODO: display userinformation
          return;
      }
      // call provider
      final String[] requieredLayer = m_layerProvider.getRequiredLayer( this );

      for( final String layerId : requieredLayer )
      {
        final IProfilChartLayer layer = m_layerProvider.createLayer( layerId, this );
        if( layer != null )
        {
          lm.addLayer( layer );
        }
      }
    }
    if( m_memento != null )
      restoreState( m_memento, lm );
  }

  /**
   * @see com.bce.profil.ui.view.IProfilView#dispose()
   */
  @Override
  public void dispose( )
  {
    if( (m_chart != null) && !m_chart.isDisposed() )
      m_chart.dispose();
    m_chart = null;

    ChartHandlerUtilities.updateElements( this );

    super.dispose();
  }

  /**
   * @see com.bce.profil.ui.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite, int)
   */
  @Override
  protected Control doCreateControl( final Composite parent, FormToolkit toolkit, final int style )
  {

    m_chart = new ChartComposite( parent, style, new ChartModel(), new RGB( 255, 255, 255 ) );
    m_chart.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    IMapperRegistry mr = m_chart.getChartModel().getMapperRegistry();

    final IAxis domainAxis = new GenericLinearAxis( ID_AXIS_DOMAIN, POSITION.BOTTOM, null );
    AxisAdjustment aaDom = new AxisAdjustment( 3, 94, 3 );
    domainAxis.setPreferredAdjustment( aaDom );

    final IAxis targetAxisLeft = new GenericLinearAxis( ID_AXIS_LEFT, POSITION.LEFT, null );
    AxisAdjustment aaLeft = new AxisAdjustment( 15, 75, 10 );
    targetAxisLeft.setPreferredAdjustment( aaLeft );

    final IAxis targetAxisRight = new GenericLinearAxis( ID_AXIS_RIGHT, POSITION.RIGHT, null );
    AxisAdjustment aaRight = new AxisAdjustment( 2, 40, 58 );
    targetAxisRight.setPreferredAdjustment( aaRight );

    domainAxis.setLabel( "[m]" );

    targetAxisLeft.setLabel( "[m+NN]" );
    targetAxisRight.setLabel( "[KS]" );

    mr.addMapper( domainAxis );
    mr.addMapper( targetAxisLeft );
    mr.addMapper( targetAxisRight );

    AxisRendererConfig configDom = new AxisRendererConfig();
    IAxisRenderer aRendDom = new GenericAxisRenderer( "rendDom", new NumberLabelCreator( "%s" ), new GenericNumberTickCalculator(), configDom );

    AxisRendererConfig configLR = new AxisRendererConfig();
    configLR.gap = AXIS_GAP;
    IAxisRenderer aRendLR = new GenericAxisRenderer( "rendLR", new NumberLabelCreator( "%s" ), new GenericNumberTickCalculator(), configLR );

    mr.setRenderer( ID_AXIS_DOMAIN, aRendDom );
    mr.setRenderer( ID_AXIS_LEFT, aRendLR );
    mr.setRenderer( ID_AXIS_RIGHT, aRendLR );

    try
    {
      createLayer();
    }
    catch( final Throwable e )
    {
      // catch everything, because if we get a runtime exception here
      // all goes down the privy...
      e.printStackTrace();
    }

    ChartUtilities.maximize( m_chart.getChartModel() );

    new ClickHandler( m_chart );

    m_plotDragHandler = new PlotDragHandlerDelegate( m_chart );
    m_axisDragHandler = new AxisDragHandlerDelegate( m_chart );
    
   

    return m_chart;
  }

  public final ICoordinateMapper getMapper( )
  {
    return m_mapper;
  }

  public ChartComposite getChart( )
  {
    return m_chart;
  }

  /**
   * @see org.eclipse.ui.IPersistableElement#getFactoryId()
   */
  public String getFactoryId( )
  {
    return null;
  }

  public IAxis getAxis( final String id )
  {
    return m_chart.getChartModel().getMapperRegistry().getAxis( id );
  }

  @Override
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    final ChartComposite chart = m_chart;
    if( (chart == null) || chart.isDisposed() )
      return;

    chart.getDisplay().syncExec( new Runnable()
    {
      public void run( )
      {
        if( hint.isObjectChanged() || hint.isPointPropertiesChanged() )
        {
          String act = null;
          for( final IChartLayer layer : chart.getChartModel().getLayerManager().getLayers() )
          {
            if( layer.isActive() )
              act = layer.getId();
          }
          createLayer();

          for( final IChartLayer layer : chart.getChartModel().getLayerManager().getLayers() )
          {
            if( act != null )
              layer.setActive( act.equals( layer.getId() ) );
          }

          return;
        }
        else if( hint.isPointsChanged() || hint.isMarkerDataChanged() || hint.isPointValuesChanged() || hint.isObjectDataChanged() || hint.isMarkerMoved() || hint.isProfilPropertyChanged()
            || hint.isActivePointChanged() || hint.isActivePropertyChanged() )
          for( final IChartLayer layer : chart.getChartModel().getLayerManager().getLayers() )
            if( layer instanceof IProfilChartLayer )
              ((IProfilChartLayer) layer).onProfilChanged( hint, changes );
        redrawChart();
      }
    } );
  }

  protected void redrawChart( )
  {
    final ChartComposite chart = m_chart;
    if( (chart != null) && !chart.isDisposed() )
      chart.getDisplay().syncExec( new Runnable()
      {

        public void run( )
        {
          chart.redraw();
        }
      } );
  }

  private final IChartLayer getActiveLayer( ILayerManager mngr )
  {
    for( final IChartLayer layer : mngr.getLayers() )
    {
      if( layer.isActive() )
        return layer;
    }
    return null;
  }

  private void restoreState( final IMemento memento, final ILayerManager mngr )
  {
    final SortedMap<Integer, String> sorted = new TreeMap<Integer, String>();
    int i = 0;
    for( final IChartLayer layer : mngr.getLayers() )
    {
      String id = layer.getId().replace( '#', '_' );//$NON-NLS-1$
      id = id.replace( ' ', '_' );//$NON-NLS-1$
      final IMemento layermem = memento.getChild( id );
      if( layermem != null )
      {
        final Boolean visible = layermem.getBoolean( MEM_LAYER_VIS );
        if( visible != null )
          layer.setVisible( visible );
        final Boolean active = layermem.getBoolean( MEM_LAYER_ACT );
        if( active != null )
        {
          layer.setActive( active );
        }
        final String data = layermem.getString( MEM_LAYER_DAT );
        if( data != null )
          layer.setData( IProfilChartLayer.VIEW_DATA_KEY, data );
        final Integer position = layermem.getInteger( MEM_LAYER_POS );
        if( position != null )
          sorted.put( position * 1000 + i++, layer.getId() );
        final IMemento childMem = layermem.getChild( MEM_LAYER_CLD );
        if( childMem != null && layer instanceof IExpandableChartLayer )
          restoreState( childMem, ((IExpandableChartLayer) layer).getLayerManager() );
      }
    }
    int pos = 0;
    for( final String layerId : sorted.values() )
    {
      mngr.moveLayerToPosition( mngr.getLayerById( layerId ), pos++ );
    }
  }

  public void restoreState( final IMemento memento )
  {
    if( m_chart == null )
      return;
    m_memento = memento;
    final ILayerManager manager = m_chart.getChartModel().getLayerManager();
    restoreState( memento, manager );
    if( getActiveLayer( manager ) == null )
      for( final IChartLayer layer : manager.getLayers() )
      {
        if( layer.isVisible() )
        {
          layer.setActive( true );
          return;
        }
      }
  }

  private IMemento getOrCreate( final IMemento memento, final String id )
  {
    final IMemento childNode = memento.getChild( id );
    return childNode == null ? memento.createChild( id ) : childNode;
  }

  private void saveState( final IMemento memento, final ILayerManager mngr )
  {
    int pos = 0;
    for( final IChartLayer layer : mngr.getLayers() )
      if( layer != null )
      {
        /**
         * you mustn't use invalid characters
         * 
         * @see com.sun.org.apache.xerces.internal.util.CoreDocumentImpl#isXMLName(String)
         */
        String sibling_ID = layer.getId().replace( '#', '_' );//$NON-NLS-1$
        sibling_ID = sibling_ID.replace( ' ', '_' );//$NON-NLS-1$

        final IMemento layermem = getOrCreate( memento, sibling_ID );
        layermem.putBoolean( MEM_LAYER_VIS, layer.isVisible() ); //$NON-NLS-1$
        layermem.putBoolean( MEM_LAYER_ACT, layer.isActive() ); //$NON-NLS-1$
        layermem.putInteger( MEM_LAYER_POS, pos++ ); //$NON-NLS-1$
        final Object data = layer.getData( IProfilChartLayer.VIEW_DATA_KEY );
        if( data != null )
        {
          layermem.putString( MEM_LAYER_DAT, data.toString() );
        }
        if( layer instanceof IExpandableChartLayer )
        {
          final IMemento childmem = getOrCreate( layermem, MEM_LAYER_CLD );
          saveState( childmem, ((IExpandableChartLayer) layer).getLayerManager() );
        }
      }
  }

  /**
   * @see org.eclipse.ui.IPersistableElement#saveState(org.eclipse.ui.IMemento)
   */
  public void saveState( final IMemento memento )
  {
    if( m_chart == null )
      return;
    saveState( memento, m_chart.getChartModel().getLayerManager() );
  }

  public IProfilLayerProvider getLayerProvider( )
  {
    return m_layerProvider;
  }

  public void setLayerProvider( IProfilLayerProvider layerProvider )
  {
    m_layerProvider = layerProvider;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.AbstractProfilView#getAdapter(java.lang.Class)
   */
  @Override
  public Object getAdapter( Class< ? > clazz )
  {
    if( clazz == IChartPart.class )
      return this;
    return super.getAdapter( clazz );
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getAxisDragHandler()
   */
  @Override
  public AxisDragHandlerDelegate getAxisDragHandler( )
  {
    return m_axisDragHandler;
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getChartComposite()
   */
  @Override
  public ChartComposite getChartComposite( )
  {
    return m_chart;
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getPlotDragHandler()
   */
  @Override
  public PlotDragHandlerDelegate getPlotDragHandler( )
  {
    return m_plotDragHandler;
  }

}