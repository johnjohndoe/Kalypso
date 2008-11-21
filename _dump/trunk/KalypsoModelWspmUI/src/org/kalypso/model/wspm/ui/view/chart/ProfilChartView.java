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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IMemento;
import org.kalypso.chart.ui.IChartPart;
import org.kalypso.chart.ui.editor.mousehandler.AxisDragHandlerDelegate;
import org.kalypso.chart.ui.editor.mousehandler.PlotDragHandlerDelegate;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilListener;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.result.IStationResult;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIExtensions;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.model.wspm.ui.profil.IProfilProviderListener;
import org.kalypso.model.wspm.ui.view.chart.handler.ProfilClickHandler;

import de.openali.odysseus.chart.ext.base.axis.GenericLinearAxis;
import de.openali.odysseus.chart.ext.base.axisrenderer.AxisRendererConfig;
import de.openali.odysseus.chart.ext.base.axisrenderer.GenericAxisRenderer;
import de.openali.odysseus.chart.ext.base.axisrenderer.GenericNumberTickCalculator;
import de.openali.odysseus.chart.ext.base.axisrenderer.NumberLabelCreator;
import de.openali.odysseus.chart.framework.model.event.impl.AbstractLayerManagerEventListener;
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
import de.openali.odysseus.chart.framework.view.impl.ChartComposite;

/**
 * @author belger
 * @author kimwerner
 */
public class ProfilChartView implements IChartPart, IProfilListener
{

  public static final int AXIS_GAP = 5; // distance between layers and Axis

  public static final String ID_AXIS_DOMAIN = "domain";//$NON-NLS-1$

  public static final String ID_AXIS_LEFT = "left";//$NON-NLS-1$

  public static final String ID_AXIS_RIGHT = "right";//$NON-NLS-1$

  private static final String MEM_LAYER_ACT = "activeLayer"; //$NON-NLS-1$

  private static final String MEM_LAYER_CLD = "layerChilds"; //$NON-NLS-1$

  private static final String MEM_LAYER_DAT = "layerData"; //$NON-NLS-1$

  private static final String MEM_LAYER_POS = "layerPosition"; //$NON-NLS-1$

  private static final String MEM_LAYER_VIS = "layerVisibility"; //$NON-NLS-1$

  private AxisDragHandlerDelegate m_axisDragHandler;

  private ChartComposite m_chartComposite = null;

  private IProfilLayerProvider m_layerProvider;

  private ICoordinateMapper m_mapper;

  private PlotDragHandlerDelegate m_plotDragHandler;

// private IMemento m_memento = null;

  private IProfil m_profile;

  private final List<IProfilProviderListener> m_listener = new ArrayList<IProfilProviderListener>();

  private IStationResult[] m_results;

// protected final Runnable m_updateControlRunnable = new Runnable()
// {
// public void run( )
// {
// updateControl();
// }
// };

// public ProfilChartView( final Composite parent )
// {
// createControl( parent, parent.getStyle() );
// }

// public ProfilChartView()
// {
// super();
// m_results = results == null ? new IStationResult[0] : results;
//
// if( getProfil() != null )
// getProfil().addProfilListener( this );
// }

  /**
   * @see org.kalypso.model.wspm.ui.view.IProfilView#createControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  public Control createControl( Composite parent )
  {
    m_chartComposite = new ChartComposite( parent, parent.getStyle(), new ChartModel(), new RGB( 255, 255, 255 ) );
    final GridData gD = new GridData( GridData.FILL_BOTH );
    gD.exclude = true;
    m_chartComposite.setLayoutData( gD );

    final IMapperRegistry mr = m_chartComposite.getChartModel().getMapperRegistry();

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

    m_chartComposite.getChartModel().getLayerManager().addListener( new AbstractLayerManagerEventListener()
    {

      /**
       * @see de.openali.odysseus.chart.framework.model.event.impl.AbstractLayerManagerEventListener#onActivLayerChanged(de.openali.odysseus.chart.framework.model.layer.IChartLayer)
       */
      @Override
      public void onActivLayerChanged( IChartLayer layer )
      {
        activeLayerChanged( layer );
      }
    } );

    new ProfilClickHandler( m_chartComposite );

    m_plotDragHandler = new PlotDragHandlerDelegate( m_chartComposite );
    m_axisDragHandler = new AxisDragHandlerDelegate( m_chartComposite );

    return m_chartComposite;
  }

  protected final void activeLayerChanged( final IChartLayer layer )
  {
// if layer is deactivated do nothing
    if( !layer.isActive() )
      return;
    // otherwise deactivate all others
    for( final IChartLayer l : m_chartComposite.getChartModel().getLayerManager().getLayers() )
    {
      if( l != layer )
        l.setActive( false );
    }
  }

//  @SuppressWarnings("boxing")//$NON-NLS-1$
// protected void createLayer( )
// {
// final ILayerManager lm = m_chartComposite.getChartModel().getLayerManager();
// IChartLayer oldActiveLayer = null;
//
// for( final IChartLayer layer : lm.getLayers() )
// {
// if( layer.isActive() )
// oldActiveLayer = layer;
// lm.removeLayer( layer );
// }
//
// if( m_profile != null )
// {
// if( m_layerProvider == null )
// {
// m_layerProvider = KalypsoModelWspmUIExtensions.createProfilLayerProvider( m_profile.getType() );
//
// if( m_layerProvider == null )
// // TODO: display userinformation
// return;
// }
// // call provider
// final String[] requieredLayer = m_layerProvider.getRequiredLayer( this );
//
// IChartLayer activeLayer = null;
// for( final String layerId : requieredLayer )
// {
// final IProfilChartLayer layer = m_layerProvider.createLayer( layerId, this );
// if( layer != null )
// {
// if( oldActiveLayer != null && layer.getId().equals( oldActiveLayer.getId() ) )
// {
// activeLayer = layer;
// }
// lm.addLayer( layer );
// }
// }
// if( activeLayer == null )
// lm.getLayers()[0].setActive( true );
// else
// activeLayer.setActive( true );
// }
// }

  public void dispose( )
  {
    if( (m_chartComposite != null) && !m_chartComposite.isDisposed() )
      m_chartComposite.dispose();
    m_axisDragHandler.dispose();
    m_plotDragHandler.dispose();

  }

  protected final IChartLayer getActiveLayer( ILayerManager mngr )
  {
    for( final IChartLayer layer : mngr.getLayers() )
    {
      if( layer.isActive() )
        return layer;
    }
    return null;
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getAdapter(java.lang.Class)
   */
  @Override
  public Object getAdapter( Class< ? > clazz )
  {
    if( IChartPart.class.equals( clazz ) )
    {
      return this;
    }

    return null;
  }

  public IAxis getAxis( final String id )
  {
    return m_chartComposite.getChartModel().getMapperRegistry().getAxis( id );
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getAxisDragHandler()
   */

  public AxisDragHandlerDelegate getAxisDragHandler( )
  {
    return m_axisDragHandler;
  }

  public ChartComposite getChart( )
  {
    return m_chartComposite;
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getChartComposite()
   */

  public ChartComposite getChartComposite( )
  {
    return m_chartComposite;
  }

  /**
   * @see org.eclipse.ui.IPersistableElement#getFactoryId()
   */
  public String getFactoryId( )
  {
    return null;
  }

  public IProfilLayerProvider getLayerProvider( )
  {
    return m_layerProvider;
  }

  public final ICoordinateMapper getMapper( )
  {
    return m_mapper;
  }

  private IMemento getOrCreate( final IMemento memento, final String id )
  {
    final IMemento childNode = memento.getChild( id );
    return childNode == null ? memento.createChild( id ) : childNode;
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getPlotDragHandler()
   */

  public PlotDragHandlerDelegate getPlotDragHandler( )
  {
    return m_plotDragHandler;
  }

  public IProfil getProfil( )
  {
    return m_profile;
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.IProfilChartViewProvider#getProfilChartView()
   */
  public ProfilChartView getProfilChartView( )
  {
    return this;
  }

  public final IStationResult[] getResults( )
  {
    if( m_results == null )
      return new IStationResult[] {};
    return m_results;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilListener#onProblemMarkerChanged(org.kalypso.model.wspm.core.profil.IProfil)
   */

  public void onProblemMarkerChanged( IProfil source )
  {
    // TODO Auto-generated method stub

  }

  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    final ChartComposite chart = m_chartComposite;
    if( (chart == null) || chart.isDisposed() )
      return;

    chart.getDisplay().syncExec( new Runnable()
    {
      public void run( )
      {
        if( hint.isObjectChanged() || hint.isPointPropertiesChanged() )
        {
          updateLayer();
        }
        else if( hint.isPointsChanged() || hint.isMarkerDataChanged() || hint.isPointValuesChanged() || hint.isObjectDataChanged() || hint.isMarkerMoved() || hint.isProfilPropertyChanged()
            || hint.isActivePointChanged() || hint.isActivePropertyChanged() )
        {
          for( final IChartLayer layer : chart.getChartModel().getLayerManager().getLayers() )
            if( layer instanceof IProfilChartLayer )
              ((IProfilChartLayer) layer).onProfilChanged( hint, changes );
          redrawChart();
        }
      }
    } );
  }

  private void fireProfilChanged( final IProfil old )
  {
    for( final IProfilProviderListener l : m_listener )
      l.onProfilProviderChanged( null, old, m_profile, null, null );
  }

  protected void redrawChart( )
  {
    final ChartComposite chart = m_chartComposite;
    if( (chart != null) && !chart.isDisposed() )
      chart.getDisplay().syncExec( new Runnable()
      {

        public void run( )
        {
          chart.redraw();
        }
      } );
  }

  public void restoreState( final IMemento memento )
  {
    if( m_chartComposite == null )
      return;
    final ILayerManager manager = m_chartComposite.getChartModel().getLayerManager();
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

  /**
   * @see org.eclipse.ui.IPersistableElement#saveState(org.eclipse.ui.IMemento)
   */

  public void saveState( final IMemento memento )
  {
    if( m_chartComposite == null )
      return;
    saveState( memento, m_chartComposite.getChartModel().getLayerManager() );
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

  public void setLayerProvider( IProfilLayerProvider layerProvider )
  {
    m_layerProvider = layerProvider;
  }

  public synchronized void setProfil( final IProfil profil )
  {
    if( m_profile == profil )
      return;

    if( m_profile != null )
      m_profile.removeProfilListener( this );

    final IProfil old = m_profile;
    m_profile = profil;
    if( m_profile == null )
    {
      ((GridData) (m_chartComposite.getLayoutData())).exclude = true;
      return;
    }
    if( m_chartComposite != null && !m_chartComposite.isDisposed() )
    {
      m_profile.addProfilListener( this );
      m_chartComposite.getChartModel().setTitle( Messages.AbstractProfilViewPart2_3 + " " + m_profile.getStation() );
      ((GridData) (m_chartComposite.getLayoutData())).exclude = false;
      updateLayer();
    }
    fireProfilChanged( old );
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.IProfilChartViewProvider#addProfilChartViewProviderListener(org.kalypso
   *      .model.wspm.ui.profil.view.chart.IProfilChartViewProviderListener)
   */
  public void addProfilProviderListener( final IProfilProviderListener l )
  {
    m_listener.add( l );
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.IProfilChartViewProvider#removeProfilChartViewProviderListener(org.
   *      kalypso.model.wspm.ui.profil.view.chart.IProfilChartViewProviderListener)
   */
  public void removeProfilProviderListener( final IProfilProviderListener l )
  {
    m_listener.remove( l );
  }

  public void updateLayer( )
  {
    if( m_layerProvider == null )
      m_layerProvider = KalypsoModelWspmUIExtensions.createProfilLayerProvider( m_profile.getType() );

    if( m_layerProvider == null )
      // TODO: display userinformation
      return;

    if( m_chartComposite != null && m_chartComposite.getChartModel() != null && m_chartComposite.getChartModel().getLayerManager() != null )
    {
      final ILayerManager lm = m_chartComposite.getChartModel().getLayerManager();
      final ArrayList<String> layers = new ArrayList<String>();
      final ArrayList<IChartLayer> layerToRemove = new ArrayList<IChartLayer>();
      final IChartLayer activeLayer = getActiveLayer( lm );
      for( final String layerId : m_layerProvider.getRequiredLayer( this ) )
        layers.add( layerId );

      // remove unused layer
      for( final IChartLayer layer : lm.getLayers() )
      {
        if( !layers.contains( layer.getId() ) )
          layerToRemove.add( layer );
      }
      for( final IChartLayer layer : layerToRemove )
        lm.removeLayer( layer );

      // add missing layer
      for( final String layerId : layers )
      {
        final IChartLayer chartLayer = lm.getLayerById( layerId );
        if( chartLayer == null )
        {
          final IProfilChartLayer profilLayer = m_layerProvider.createLayer( layerId, this );
          if( profilLayer != null )
          {
            lm.addLayer( profilLayer );
          }
        }
        else if( chartLayer instanceof IProfilChartLayer )
        {
          final IProfilChartLayer profilLayer = (IProfilChartLayer) chartLayer;
          profilLayer.setProfil( getProfil() );
        }
      }

      // activate first layer
      if( activeLayer == null && lm.getLayers().length > 0 )
      {
        lm.getLayers()[0].setActive( true );
        activeLayerChanged( lm.getLayers()[0] );
        return;
      }

      // reactivate layer
      final IChartLayer layer = lm.getLayerById( activeLayer == null ? "" : activeLayer.getId() );
      if( layer != null )
      {
        layer.setActive( true );
        activeLayerChanged( layer );
        return;
      }
      // old active Layer removed
      if( lm.getLayers().length > 0 )
      {
        lm.getLayers()[0].setActive( true );
        activeLayerChanged( lm.getLayers()[0] );
      }

    }
  }

// private void fireOnProfilChartViewChanged( )
// {
// for( final IProfilChartViewProviderListener l : m_listener )
// l.onProfilChartViewChanged( getProfilChartView() );
// }

// public void updateControl( )
// {
//
//    
// if( m_chart == null || m_chart.isDisposed() )
// return;
//
//
// if( m_profile == null )
// {
// final Label label = new Label( m_chart, SWT.CENTER );
// label.setText( Messages.AbstractProfilPart_0 );
// final GridData gridData = new GridData();
// gridData.grabExcessHorizontalSpace = true;
// gridData.horizontalAlignment = SWT.FILL;
// gridData.horizontalIndent = 10;
// gridData.grabExcessVerticalSpace = true;
// gridData.verticalAlignment = SWT.CENTER;
// gridData.verticalIndent = 10;
//
// label.setLayoutData( gridData );
// }
// else
// {
// // final ArrayList<String> kommentare = (ArrayList<String>) m_pem.getProfil().getProperty(
// // PROFIL_PROPERTY.KOMMENTAR );
//
// // setContentDescription( (kommentare == null) ? "" : kommentare.toString() );
//      
//     
//      
//      
// m_chart = new ProfilChartView( m_profile );// , m_profilColorRegistry );
// m_chartview.setLayerProvider( m_layerProvider );
// m_chartview.createControl( m_control, SWT.BORDER );
// m_chartview.restoreState( m_viewdata.getChartMemento() );
//
// // final Control chartControl = m_chartview.getControl();
// // chartControl.setMenu( m_menuManager.createContextMenu( chartControl ) );
// }
//
// m_control.layout();
//
// fireOnProfilChartViewChanged();
// }
}