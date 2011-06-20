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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.awt.Insets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.kalypso.chart.ui.IChartPart;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilListener;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIExtensions;
import org.kalypso.model.wspm.ui.profil.IProfilProviderListener;
import org.kalypso.model.wspm.ui.view.chart.IProfilChart;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider;

import de.openali.odysseus.chart.framework.model.IChartModel;
import de.openali.odysseus.chart.framework.model.event.IChartModelEventListener;
import de.openali.odysseus.chart.framework.model.event.impl.AbstractLayerManagerEventListener;
import de.openali.odysseus.chart.framework.model.impl.ChartModel;
import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILayerManager;
import de.openali.odysseus.chart.framework.model.mapper.IAxis;
import de.openali.odysseus.chart.framework.model.mapper.IAxisConstants.ALIGNMENT;
import de.openali.odysseus.chart.framework.model.mapper.registry.IMapperRegistry;
import de.openali.odysseus.chart.framework.util.StyleUtils;
import de.openali.odysseus.chart.framework.view.IChartComposite;
import de.openali.odysseus.chart.framework.view.IPlotHandler;
import de.openali.odysseus.chart.framework.view.impl.ChartImageComposite;

/**
 * @author belger
 * @author kimwerner
 * 
 * @deprecated use ProfilChartViewPart or ProfilChartComposite instead
 */
@Deprecated
public class ProfilChartView implements IChartPart, IProfilListener, IProfilChart
{

  private IChartComposite m_chartComposite = null;

  private IProfilLayerProvider m_layerProvider;

  private final List<IProfilProviderListener> m_listener = new ArrayList<IProfilProviderListener>();

  private IProfil m_profile;

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

//  private void setDefaultAxis( final IMapperRegistry mr )
//  {
//    final AxisRendererConfig configDom = new AxisRendererConfig();
//    final IAxisRenderer aRendDom = new ExtendedAxisRenderer( "rendDom", POSITION.BOTTOM, new NumberLabelCreator( "%s" ), new GenericNumberTickCalculator(), configDom ); //$NON-NLS-1$ //$NON-NLS-2$
//
//    final AxisRendererConfig configLR = new AxisRendererConfig();
//    configLR.axisInsets = new Insets( 5, 0, 0, 0 );
//    final IAxisRenderer aRendL = new ExtendedAxisRenderer( "rendL", POSITION.LEFT, new NumberLabelCreator( "%s" ), new GenericNumberTickCalculator(), configLR ); //$NON-NLS-1$ //$NON-NLS-2$
//    final IAxisRenderer aRendR = new ExtendedAxisRenderer( "rendL", POSITION.RIGHT, new NumberLabelCreator( "%s" ), new GenericNumberTickCalculator(), configLR ); //$NON-NLS-1$ //$NON-NLS-2$
//
//    final IAxis domainAxis = new GenericLinearAxis( "ID_AXIS_DOMAIN", POSITION.BOTTOM, null, aRendDom );//$NON-NLS-1$
//    final AxisAdjustment aaDom = new AxisAdjustment( 3, 94, 3 );
//    domainAxis.setPreferredAdjustment( aaDom );
//
//    final IAxis targetAxisLeft = new GenericLinearAxis( "ID_AXIS_LEFT", POSITION.LEFT, null, aRendL );//$NON-NLS-1$
//    final AxisAdjustment aaLeft = new AxisAdjustment( 15, 75, 10 );
//    targetAxisLeft.setPreferredAdjustment( aaLeft );
//
//    final IAxis targetAxisRight = new GenericLinearAxis( "ID_AXIS_RIGHT", POSITION.RIGHT, null, aRendR );//$NON-NLS-1$
//    final AxisAdjustment aaRight = new AxisAdjustment( 2, 40, 58 );
//    targetAxisRight.setPreferredAdjustment( aaRight );
//
//    domainAxis.setLabel( "[m]" ); //$NON-NLS-1$
//
//    targetAxisLeft.setLabel( "[m+NN]" ); //$NON-NLS-1$
//    targetAxisRight.setLabel( "[KS]" ); //$NON-NLS-1$
//
//    mr.addMapper( domainAxis );
//    mr.addMapper( targetAxisLeft );
//    mr.addMapper( targetAxisRight );
//  }

  /**
   * @see org.kalypso.model.wspm.ui.view.IProfilView#createControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  public Control createControl( final Composite parent )
  {
    m_chartComposite = new ChartImageComposite( parent, parent.getStyle(), new ChartModel(), new RGB( 255, 255, 255 ) );
    final GridData gD = new GridData( SWT.FILL, SWT.FILL, true, true );
    // gD.exclude = true;
    m_chartComposite.getPlot().setLayoutData( gD );
    m_chartComposite.getChartModel().getBehaviour().setHideUnusedAxes( true );

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

    updateLayer();

    return m_chartComposite.getPlot();
  }

  public void dispose( )
  {
    if( m_chartComposite != null && !m_chartComposite.getPlot().isDisposed() )
      m_chartComposite.getPlot().dispose();
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
      map.put( layer.getIdentifier(), layer.isVisible() );
      saveStateVisible( layer.getLayerManager(), map );
    }
  }

  private final List<Object> saveStatePosition( final ILayerManager mngr )
  {
    final List<Object> list = new ArrayList<Object>();

    for( final IChartLayer layer : mngr.getLayers() )
    {
      list.add( layer.getIdentifier() );
      final List<Object> subList = saveStatePosition( layer.getLayerManager() );
      list.add( subList );
    }

    return list;
  }

  private final String saveStateActive( final ILayerManager mngr )
  {
    for( final IChartLayer layer : mngr.getLayers() )
    {
      if( layer.isActive() )
        return layer.getIdentifier();
    }
    return ""; //$NON-NLS-1$
  }

  private final void restoreStateVisible( final ILayerManager mngr, final HashMap<String, Boolean> map )
  {
    for( final IChartLayer layer : mngr.getLayers() )
    {
      final Boolean visible = map.get( layer.getIdentifier() );
      if( visible != null )
      {
        layer.setVisible( visible );
        restoreStateVisible( layer.getLayerManager(), map );
      }
    }
  }

  @SuppressWarnings("unchecked")
  private final void restoreStatePosition( final ILayerManager mngr, final List<Object> list )
  {
    if( mngr == null )
      return;

    int pos = 0;
    for( final Object o : list )
    {
      if( o instanceof List )
      {
        final List<Object> l = (ArrayList<Object>) o;
        final Object id = l.get( 0 );
        final IChartLayer layer = id == null ? null : mngr.findLayer( id.toString() );
        if( layer != null )
        {
          mngr.moveLayerToPosition( layer, pos++ );
          restoreStatePosition( layer.getLayerManager(), l );
        }
      }
      else
      {
        final IChartLayer layer = mngr.findLayer( o.toString() );
        if( layer != null )
        {
          mngr.moveLayerToPosition( layer, pos++ );
        }
      }
    }
  }

  private final void restoreStateActive( final ILayerManager mngr, final String id )
  {
    final IChartLayer layer = mngr.findLayer( id );
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

  public IAxis getAxis( final String id )
  {
    return m_chartComposite.getChartModel().getMapperRegistry().getAxis( id );
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getAxisDragHandler()
   */

  @Override
  public IChartComposite getChart( )
  {
    return m_chartComposite;
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getChartComposite()
   */
  @Override
  public IChartComposite getChartComposite( )
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

  @Override
  public IPlotHandler getPlotDragHandler( )
  {
    return m_chartComposite.getPlotHandler();
  }

  @Override
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

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilListener#onProblemMarkerChanged(org.kalypso.model.wspm.core.profil.IProfil)
   */

  @Override
  public void onProblemMarkerChanged( final IProfil source )
  {
    // TODO Auto-generated method stub

  }

  @Override
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    final IChartComposite chart = m_chartComposite;
    if( chart == null || chart.getPlot().isDisposed() )
      return;

    chart.getPlot().getDisplay().syncExec( new Runnable()
    {
      @Override
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
          {
            ((IProfilChartLayer) layer).onProfilChanged( hint, changes );
          }
        }
        redrawChart();
      }
    } );
  }

  protected void redrawChart( )
  {
    final IChartComposite chart = m_chartComposite;
    if( chart != null && !chart.getPlot().isDisposed() )
      chart.getPlot().getDisplay().syncExec( new Runnable()
      {

        @Override
        public void run( )
        {
          chart.getPlot().redraw();
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

  @Override
  public synchronized void setProfil( final IProfil profil, final Object result )
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
      ((GridData) m_chartComposite.getPlot().getLayoutData()).exclude = true;
      m_chartComposite.getChartModel().getSettings().setTitle( "<No Profile Selected>", ALIGNMENT.CENTER, StyleUtils.getDefaultTextStyle(), new Insets( 0, 0, 0, 0 ) ); //$NON-NLS-1$

    }
    else
    {
      if( m_chartComposite != null && !m_chartComposite.getPlot().isDisposed() )
      {
        m_profile.addProfilListener( this );

        m_chartComposite.getChartModel().getSettings().setTitle( String.format( "Station km %10.4f", m_profile.getStation() ), ALIGNMENT.CENTER, StyleUtils.getDefaultTextStyle(), new Insets( 0, 0, 0, 0 ) );
        ((GridData) m_chartComposite.getPlot().getLayoutData()).exclude = false;
        updateLayer();
      }
    }

    fireProfilChanged( old );
  }

  public void updateLayer( )
  {
    if( m_layerProvider == null && m_profile != null )
      m_layerProvider = KalypsoModelWspmUIExtensions.createProfilLayerProvider( m_profile.getType() );

    // TODO: display userinformation
    if( m_layerProvider == null )
      return;

    if( m_chartComposite == null )
      return;

    final IChartModel chartModel = m_chartComposite.getChartModel();
    if( chartModel == null )
      return;

    final IMapperRegistry mr = chartModel.getMapperRegistry();
    final IAxis[] existingAxis = mr.getAxes();
    if( existingAxis == null || existingAxis.length == 0 )
    {
      // /* Register default axis and axis renderer. */
      final IAxis[] axis = m_layerProvider.registerAxis( mr );
      // if( axis.length > 0 )
      // m_layerProvider.registerAxisRenderer( mr );
      // else
      // {
      // /* Register default axis and axis renderer. */
      // setDefaultAxis( mr );
      // }
    }

    if( m_chartComposite != null && m_chartComposite.getChartModel() != null && m_chartComposite.getChartModel().getLayerManager() != null )
    {
      final ILayerManager lm = m_chartComposite.getChartModel().getLayerManager();

      // saveState
      final String activeLayerId = saveStateActive( lm );
      final HashMap<String, Boolean> visibility = new HashMap<String, Boolean>();
      saveStateVisible( lm, visibility );
      final List<Object> positions = saveStatePosition( lm );
      // remove layer
      for( final IChartLayer layer : lm.getLayers() )
        lm.removeLayer( layer );

      // add layer
      if( m_profile != null )
      {
        final IProfilChartLayer[] profileLayers = m_layerProvider.createLayers( m_profile, null );
        for( final IProfilChartLayer layer : profileLayers )
          lm.addLayer( layer );
      }

      restoreStatePosition( lm, positions );
      restoreStateVisible( lm, visibility );
      restoreStateActive( lm, activeLayerId );
      //
      // m_chartComposite.getPlot().invalidate( lm.getLayers() );
    }

  }

  /**
   * @see de.openali.odysseus.chart.framework.model.event.IEventProvider#addListener(java.lang.Object)
   */
  @Override
  public void addListener( final IChartModelEventListener listener )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see de.openali.odysseus.chart.framework.model.event.IEventProvider#removeListener(java.lang.Object)
   */
  @Override
  public void removeListener( final IChartModelEventListener listener )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getOutlinePage()
   */
  @Override
  public IContentOutlinePage getOutlinePage( )
  {
    // TODO Auto-generated method stub
    return null;
  }
}