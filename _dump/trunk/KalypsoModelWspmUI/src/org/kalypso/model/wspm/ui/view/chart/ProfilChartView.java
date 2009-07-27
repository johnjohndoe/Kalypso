/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
import java.util.HashMap;
import java.util.List;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.chart.ui.IChartPart;
import org.kalypso.chart.ui.editor.mousehandler.AxisDragHandlerDelegate;
import org.kalypso.chart.ui.editor.mousehandler.PlotDragHandlerDelegate;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilListener;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.result.IStationResult;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIExtensions;
import org.kalypso.model.wspm.ui.i18n.Messages;
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
// public static final int AXIS_GAP = 5; // distance between layers and Axis

//  public static final String ID_AXIS_DOMAIN = "domain";//$NON-NLS-1$
//
//  public static final String ID_AXIS_LEFT = "left";//$NON-NLS-1$
//
//  public static final String ID_AXIS_RIGHT = "right";//$NON-NLS-1$

  private AxisDragHandlerDelegate m_axisDragHandler;

  private ChartComposite m_chartComposite = null;

  private IProfilLayerProvider m_layerProvider;

  private final List<IProfilProviderListener> m_listener = new ArrayList<IProfilProviderListener>();

  private PlotDragHandlerDelegate m_plotDragHandler;

  private IProfil m_profile;

  private IStationResult[] m_results;

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

  public void addProfilProviderListener( final IProfilProviderListener l )
  {
    m_listener.add( l );
  }

  private void setDefaultAxis( final IMapperRegistry mr )
  {
    final IAxis domainAxis = new GenericLinearAxis( "ID_AXIS_DOMAIN", POSITION.BOTTOM, null );//$NON-NLS-1$
    final AxisAdjustment aaDom = new AxisAdjustment( 3, 94, 3 );
    domainAxis.setPreferredAdjustment( aaDom );

    final IAxis targetAxisLeft = new GenericLinearAxis( "ID_AXIS_LEFT", POSITION.LEFT, null );//$NON-NLS-1$
    final AxisAdjustment aaLeft = new AxisAdjustment( 15, 75, 10 );
    targetAxisLeft.setPreferredAdjustment( aaLeft );

    final IAxis targetAxisRight = new GenericLinearAxis( "ID_AXIS_RIGHT", POSITION.RIGHT, null );//$NON-NLS-1$
    final AxisAdjustment aaRight = new AxisAdjustment( 2, 40, 58 );
    targetAxisRight.setPreferredAdjustment( aaRight );

    domainAxis.setLabel( "[m]" ); //$NON-NLS-1$

    targetAxisLeft.setLabel( "[m+NN]" ); //$NON-NLS-1$
    targetAxisRight.setLabel( "[KS]" ); //$NON-NLS-1$

    mr.addMapper( domainAxis );
    mr.addMapper( targetAxisLeft );
    mr.addMapper( targetAxisRight );

    final AxisRendererConfig configDom = new AxisRendererConfig();
    final IAxisRenderer aRendDom = new GenericAxisRenderer( "rendDom", new NumberLabelCreator( "%s" ), new GenericNumberTickCalculator(), configDom ); //$NON-NLS-1$ //$NON-NLS-2$

    final AxisRendererConfig configLR = new AxisRendererConfig();
    configLR.gap = 5;
    final IAxisRenderer aRendLR = new GenericAxisRenderer( "rendLR", new NumberLabelCreator( "%s" ), new GenericNumberTickCalculator(), configLR ); //$NON-NLS-1$ //$NON-NLS-2$

    mr.setRenderer( "ID_AXIS_DOMAIN", aRendDom );//$NON-NLS-1$
    mr.setRenderer( "ID_AXIS_LEFT", aRendLR );//$NON-NLS-1$
    mr.setRenderer( "ID_AXIS_RIGHT", aRendLR );//$NON-NLS-1$
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.IProfilView#createControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  public Control createControl( final Composite parent )
  {
    m_chartComposite = new ChartComposite( parent, parent.getStyle(), new ChartModel(), new RGB( 255, 255, 255 ) );
    final GridData gD = new GridData( GridData.FILL_BOTH );
    gD.exclude = true;
    m_chartComposite.setLayoutData( gD );

    m_chartComposite.getChartModel().getLayerManager().addListener( new AbstractLayerManagerEventListener()
    {

      /**
       * @see de.openali.odysseus.chart.framework.model.event.impl.AbstractLayerManagerEventListener#onActivLayerChanged(de.openali.odysseus.chart.framework.model.layer.IChartLayer)
       */
      @Override
      public void onActivLayerChanged( final IChartLayer layer )
      {
        activeLayerChanged( layer );
      }
    } );

    new ProfilClickHandler( m_chartComposite );

    m_plotDragHandler = new PlotDragHandlerDelegate( m_chartComposite );
    m_axisDragHandler = new AxisDragHandlerDelegate( m_chartComposite );

    return m_chartComposite;
  }

  public void dispose( )
  {
    if( (m_chartComposite != null) && !m_chartComposite.isDisposed() )
      m_chartComposite.dispose();

    if( m_axisDragHandler != null )
      m_axisDragHandler.dispose();

    if( m_plotDragHandler != null )
      m_plotDragHandler.dispose();

  }

  private void fireProfilChanged( final IProfil old )
  {
    for( final IProfilProviderListener l : m_listener )
      l.onProfilProviderChanged( null, old, m_profile );
  }

  private final void saveStateVisible( final ILayerManager mngr, final HashMap<String, Boolean> map )
  {
    for( final IChartLayer layer : mngr.getLayers() )
    {
      map.put( layer.getId(), layer.isVisible() );
      if( layer instanceof IExpandableChartLayer )
      {
        saveStateVisible( ((IExpandableChartLayer) layer).getLayerManager(), map );
      }
    }
  }

  private final void saveStatePosition( final ILayerManager mngr, final ArrayList<Object> list )
  {
    for( final IChartLayer layer : mngr.getLayers() )
    {
      if( layer instanceof IExpandableChartLayer )
      {
        final ArrayList<Object> l = new ArrayList<Object>();
        l.add( layer.getId() );
        saveStatePosition( ((IExpandableChartLayer) layer).getLayerManager(), l );
        list.add( l );
      }
      else
        list.add( layer.getId() );
    }
  }

  private final String saveStateActive( final ILayerManager mngr )
  {
    for( final IChartLayer layer : mngr.getLayers() )
    {
      if( layer.isActive() )
        return layer.getId();
    }
    return "";
  }

  private final void restoreStateVisible( final ILayerManager mngr, final HashMap<String, Boolean> map )
  {
    for( final IChartLayer layer : mngr.getLayers() )
    {
      final Boolean visible = map.get( layer.getId() );
      if( visible != null )
        layer.setVisible( visible );
      if( layer instanceof IExpandableChartLayer )
      {
        restoreStateVisible( ((IExpandableChartLayer) layer).getLayerManager(), map );
      }
    }
  }

  @SuppressWarnings("unchecked")
  private final void restoreStatePosition( final ILayerManager mngr, final ArrayList<Object> list )
  {
    int pos = 0;
    for( final Object o : list )
    {
      if( o instanceof ArrayList )
      {
        final ArrayList<Object> l = (ArrayList<Object>) o;
        final Object id = l.get( 0 );
        final IChartLayer layer = mngr.getLayerById( id.toString() );
        if( layer != null )
        {
          mngr.moveLayerToPosition( layer, pos++ );
          if( layer instanceof IExpandableChartLayer )
          {
            restoreStatePosition( ((IExpandableChartLayer) layer).getLayerManager(), l );
          }
        }
      }
      else
      {
        final IChartLayer layer = mngr.getLayerById( o.toString() );
        if( layer != null )
        {
          mngr.moveLayerToPosition( layer, pos++ );
        }
      }
    }
  }

  private final void restoreStateActive( final ILayerManager mngr, final String id )
  {
    final IChartLayer layer = mngr.getLayerById( id );
    if( layer != null )
    {
      layer.setActive( true );
      activeLayerChanged( layer );
      return;
    }

// old active Layer removed
    if( mngr.getLayers().length > 0 )
    {
      mngr.getLayers()[0].setActive( true );
      activeLayerChanged( mngr.getLayers()[0] );
    }
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getAdapter(java.lang.Class)
   */
  @Override
  public Object getAdapter( final Class< ? > clazz )
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

  public void onProblemMarkerChanged( final IProfil source )
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
            {
              // TODO: Kim, wechsel der Achsentexte ermöglichen
// final ICoordinateMapper cm = layer.getCoordinateMapper();
// if( cm != null && (cm.getTargetAxis() == getAxis( ID_AXIS_RIGHT )) )
// {
// final IComponent cp = ((IProfilChartLayer) layer).getTargetComponent();
// if( cp != null )
// if( !getAxis( ID_AXIS_RIGHT ).getLabel().equals( "[" + cp.getUnit() + "]" ) )
// {
// getAxis( ID_AXIS_RIGHT ).setLabel( "[" + cp.getUnit() + "]" );
// final IAxisComponent ac = chart.getChartModel().getMapperRegistry().getComponent( getAxis( ID_AXIS_RIGHT ) );
// ((AxisCanvas) ac).layout();
// }
// }
              ((IProfilChartLayer) layer).onProfilChanged( hint, changes );
            }
          redrawChart();
        }
      }
    } );
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

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.IProfilChartViewProvider#removeProfilChartViewProviderListener(org.
   *      kalypso.model.wspm.ui.profil.view.chart.IProfilChartViewProviderListener)
   */
  public void removeProfilProviderListener( final IProfilProviderListener l )
  {
    m_listener.remove( l );
  }

  public void setLayerProvider( final IProfilLayerProvider layerProvider )
  {
    m_layerProvider = layerProvider;
  }

  public synchronized void setProfil( final IProfil profil )
  {
    if( m_profile == profil )
      return;

    if( m_profile != null )
    {
      m_profile.removeProfilListener( this );

    }

    final IProfil old = m_profile;
    m_profile = profil;
    if( m_profile == null )
    {
      ((GridData) (m_chartComposite.getLayoutData())).exclude = true;

      final ILayerManager lm = m_chartComposite.getChartModel().getLayerManager();
      lm.dispose();

      return;
    }
    if( m_chartComposite != null && !m_chartComposite.isDisposed() )
    {
      m_profile.addProfilListener( this );
      m_chartComposite.getChartModel().setTitle( Messages.getFormatString( "org.kalypso.model.wspm.ui.view.AbstractProfilViewPart_3", m_profile.getStation() ) ); //$NON-NLS-1$
      ((GridData) (m_chartComposite.getLayoutData())).exclude = false;
      updateLayer();
    }
    fireProfilChanged( old );
  }

  public void updateLayer( )
  {
    if( m_layerProvider == null )
    {
      m_layerProvider = KalypsoModelWspmUIExtensions.createProfilLayerProvider( m_profile.getType() );
      final IMapperRegistry mr = m_chartComposite.getChartModel().getMapperRegistry();
      final IAxis[] axis = m_layerProvider.registerAxis( mr );
      if( axis.length == 0 )
        setDefaultAxis( mr );
      else
        m_layerProvider.registerAxisRenderer( mr );

    }
    // TODO: display userinformation
    if( m_layerProvider == null )
      return;

    if( m_chartComposite != null && m_chartComposite.getChartModel() != null && m_chartComposite.getChartModel().getLayerManager() != null )
    {
      final ILayerManager lm = m_chartComposite.getChartModel().getLayerManager();

      // saveState
      final String activeLayerId = saveStateActive( lm );
      final HashMap<String, Boolean> visibility = new HashMap<String, Boolean>();
      saveStateVisible( lm, visibility );
      final ArrayList<Object> positions = new ArrayList<Object>();
      saveStatePosition( lm, positions );
      // remove layer
      for( final IChartLayer layer : lm.getLayers() )
        lm.removeLayer( layer );

      // add layer
      for( final String layerId : m_layerProvider.getRequiredLayer( this ) )
      {
        final IProfilChartLayer profilLayer = m_layerProvider.createLayer( layerId, this );
        if( profilLayer != null )
          lm.addLayer( profilLayer );
      }

      restoreStatePosition( lm, positions );
      restoreStateVisible( lm, visibility );
      restoreStateActive( lm, activeLayerId );

      // activate first layer
// if( activeLayer == null && lm.getLayers().length > 0 )
// {
// lm.getLayers()[0].setActive( true );
// activeLayerChanged( lm.getLayers()[0] );
// return;
// }
//
// // reactivate layer
// final IChartLayer layer = lm.getLayerById( activeLayerId );
// if( layer != null )
// {
// layer.setActive( true );
// activeLayerChanged( layer );
// return;
// }
//
// // old active Layer removed
// if( lm.getLayers().length > 0 )
// {
// lm.getLayers()[0].setActive( true );
// activeLayerChanged( lm.getLayers()[0] );
// }
    }
  }
}