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
package org.kalypso.model.wspm.ui.view;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.contribs.eclipse.swt.custom.ScrolledCompositeCreator;
import org.kalypso.contribs.eclipse.ui.partlistener.AdapterPartListener;
import org.kalypso.contribs.eclipse.ui.partlistener.EditorFirstAdapterFinder;
import org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartViewProvider;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartViewProviderListener;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;

import de.openali.odysseus.chart.framework.model.IChartModel;
import de.openali.odysseus.chart.framework.model.event.ILayerManagerEventListener;
import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILayerManager;
import de.openali.odysseus.chart.framework.view.impl.ChartComposite;

/**
 * @author Gernot Belger
 */
@SuppressWarnings("unchecked")
public class LayerView extends ViewPart implements IAdapterEater, IProfilChartViewProviderListener, ILayerManagerEventListener// ,

{
  /**
   * @see de.openali.odysseus.chart.framework.model.event.ILayerManagerEventListener#onActivLayerChanged(de.openali.odysseus.chart.framework.model.layer.IChartLayer)
   */
  @Override
  public void onActivLayerChanged( IChartLayer layer )
  {
    if( layer.isActive() )
      updatePanel( layer );
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.event.ILayerManagerEventListener#onLayerAdded(de.openali.odysseus.chart.framework.model.layer.IChartLayer)
   */
  @Override
  public void onLayerAdded( IChartLayer layer )
  {
    for( final IChartLayer l : m_layerManager.getLayers() )
    {
      l.setActive( l == layer );
    }
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.event.ILayerManagerEventListener#onLayerContentChanged(de.openali.odysseus.chart.framework.model.layer.IChartLayer)
   */
  @Override
  public void onLayerContentChanged( IChartLayer layer )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see de.openali.odysseus.chart.framework.model.event.ILayerManagerEventListener#onLayerMoved(de.openali.odysseus.chart.framework.model.layer.IChartLayer)
   */
  @Override
  public void onLayerMoved( IChartLayer layer )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see de.openali.odysseus.chart.framework.model.event.ILayerManagerEventListener#onLayerRemoved(de.openali.odysseus.chart.framework.model.layer.IChartLayer)
   */
  @Override
  public void onLayerRemoved( IChartLayer layer )
  {
    final IChartLayer[] layers = m_layerManager.getLayers();
    if( layers == null || layers.length < 1 )
      return;
    if( layer.isActive() )
      m_layerManager.getLayers()[0].setActive( true );
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.event.ILayerManagerEventListener#onLayerVisibilityChanged(de.openali.odysseus.chart.framework.model.layer.IChartLayer)
   */
  @Override
  public void onLayerVisibilityChanged( IChartLayer layer )
  {
    // TODO Auto-generated method stub

  }

  private final ScrolledCompositeCreator m_creator = new ScrolledCompositeCreator( null )
  {
    @Override
    protected Control createContents( final Composite sc, final int style )
    {
      final Group group = new Group( sc, style );
      group.setLayoutData( new GridData( GridData.FILL_BOTH ) );
      group.setLayout( new GridLayout() );

      return group;
    }
  };

  private final AdapterPartListener m_providerListener = new AdapterPartListener( IProfilChartViewProvider.class, this, EditorFirstAdapterFinder.instance(), EditorFirstAdapterFinder.instance() );

  private IProfilChartViewProvider m_provider;

  private IChartLayer m_activeLayer;

  private ILayerManager m_layerManager = null;

  /**
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite)
   */
  @Override
  public void init( IViewSite site ) throws PartInitException
  {
    super.init( site );

    m_providerListener.init( site.getPage() );
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#dispose()
   */
  @Override
  public void dispose( )
  {
    unhookProvider();

    m_providerListener.dispose();

    super.dispose();
  }

  private void unhookProvider( )
  {
    if( m_provider != null )
    {

      m_provider = null;
    }
    if( m_layerManager != null )
      m_layerManager.removeListener( this );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    final Control control = m_creator.getContentControl();
    if( control != null )
      control.setFocus();
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    m_creator.createControl( parent, SWT.H_SCROLL | SWT.V_SCROLL, SWT.NONE );
    if( m_provider != null )
      onProfilChartViewChanged( m_provider.getProfilChartView() );
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater#setAdapter(java.lang.Object)
   */
  public void setAdapter( final IWorkbenchPart part, final Object adapter )
  {
    final IProfilChartViewProvider provider = (IProfilChartViewProvider) adapter;

    if( m_provider == provider && provider != null )
      return;

    unhookProvider();

    m_provider = provider;

    if( m_provider != null )
    {
      m_provider.addProfilChartViewProviderListener( this );
      onProfilChartViewChanged( m_provider.getProfilChartView() );
    }

  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartViewProviderListener#onProfilChartViewChanged(org.kalypso.model.wspm.ui.view.chart.ProfilChartView)
   */
  @Override
  public void onProfilChartViewChanged( ProfilChartView newProfilChartView )
  {
    final ChartComposite chart = newProfilChartView == null ? null : newProfilChartView.getChart();
    final IChartModel model = chart == null ? null : chart.getChartModel();
    m_layerManager = model == null ? null : model.getLayerManager();
    if( m_layerManager != null )
      m_layerManager.addListener( this );
    final IChartLayer[] layers = m_layerManager == null ? new IChartLayer[] {} : m_layerManager.getLayers();
    IChartLayer activeLayer = null;
    for( final IChartLayer layer : layers )
      if( layer.isActive() )
      {
        activeLayer = layer;
        break;
      }
    if( m_activeLayer == activeLayer )
      return;
    final Group group = (Group) m_creator.getContentControl();
    if( group == null || group.isDisposed() )
      return;

    for( final Control c : group.getChildren() )
    {
      if( !c.isDisposed() )
        c.dispose();
    }
    group.setText( "" ); //$NON-NLS-1$

    updatePanel( activeLayer );

// }
// else
// {
// // alles gescheitert -> Meldung
// final Label label = new Label( group, SWT.CENTER );
// final GridData gridData = new GridData();
// gridData.grabExcessHorizontalSpace = true;
// gridData.horizontalAlignment = SWT.FILL;
// gridData.horizontalIndent = 10;
// gridData.grabExcessVerticalSpace = true;
// gridData.verticalAlignment = SWT.CENTER;
// gridData.verticalIndent = 10;
//
// label.setLayoutData( gridData );
// label.setText( Messages.LayerView_1 );
// }
//
// group.layout();
// m_creator.updateControlSize( false );
  }

// /**
// * @see de.belger.swtchart.layer.IActiveLayerChangeListener#onActiveLayerChanged(de.belger.swtchart.layer.IChartLayer)
// */
// public void onActiveLayerChanged( final IChartLayer activeLayer )
// {
//
// if( m_activeLayer == activeLayer )
// return;
// final Group group = (Group) m_creator.getContentControl();
// if( group == null || group.isDisposed() )
// return;
//
// for( final Control c : group.getChildren() )
// {
// if( !c.isDisposed() )
// c.dispose();
// }
//    group.setText( "" ); //$NON-NLS-1$
//
// if( activeLayer != null )
// {
// group.setText( activeLayer.getTitle() );
// final IProfilView panel = activeLayer instanceof IProfilChartLayer ? ((IProfilChartLayer)
  // activeLayer).createLayerPanel() : null;
//
// if( panel != null )
// {
// final Control control = panel.createControl( group, SWT.NONE );
// control.setLayoutData( new GridData( GridData.FILL_BOTH ) );
// }
// else
// new Label( group, SWT.NONE | SWT.WRAP );
//
// m_activeLayer = activeLayer;
// }
// else
// {
// // alles gescheitert -> Meldung
// final Label label = new Label( group, SWT.CENTER );
// final GridData gridData = new GridData();
// gridData.grabExcessHorizontalSpace = true;
// gridData.horizontalAlignment = SWT.FILL;
// gridData.horizontalIndent = 10;
// gridData.grabExcessVerticalSpace = true;
// gridData.verticalAlignment = SWT.CENTER;
// gridData.verticalIndent = 10;
//
// label.setLayoutData( gridData );
// label.setText( Messages.LayerView_1 );
// }
//
// group.layout();
// m_creator.updateControlSize( false );
// }
  final void updatePanel( final IChartLayer activeLayer )
  {

// group.setText( activeLayer.getTitle() );
// final IProfilView panel = activeLayer instanceof IProfilChartLayer ? ((IProfilChartLayer)
    // activeLayer).createLayerPanel() : null;
//
// if( panel != null )
// {
// final Control control = panel.createControl( group, SWT.NONE );
// control.setLayoutData( new GridData( GridData.FILL_BOTH ) );
// }
// else
// new Label( group, SWT.NONE | SWT.WRAP );
//
// m_activeLayer = activeLayer;

    if( m_activeLayer == activeLayer )
      return;
    final Group group = (Group) m_creator.getContentControl();
    if( group == null || group.isDisposed() )
      return;

    for( final Control c : group.getChildren() )
    {
      if( !c.isDisposed() )
        c.dispose();
    }
    group.setText( "" ); //$NON-NLS-1$

    if( activeLayer != null )
    {
      group.setText( activeLayer.getTitle() );
      final IProfilView panel = activeLayer instanceof IProfilChartLayer ? ((IProfilChartLayer) activeLayer).createLayerPanel() : null;

      if( panel != null )
      {
        final Control control = panel.createControl( group, SWT.NONE );
        control.setLayoutData( new GridData( GridData.FILL_BOTH ) );
      }
      else
        new Label( group, SWT.NONE | SWT.WRAP );

      m_activeLayer = activeLayer;
    }
    else
    {
// alles gescheitert -> Meldung
      final Label label = new Label( group, SWT.CENTER );
      final GridData gridData = new GridData();
      gridData.grabExcessHorizontalSpace = true;
      gridData.horizontalAlignment = SWT.FILL;
      gridData.horizontalIndent = 10;
      gridData.grabExcessVerticalSpace = true;
      gridData.verticalAlignment = SWT.CENTER;
      gridData.verticalIndent = 10;

      label.setLayoutData( gridData );
      label.setText( Messages.LayerView_1 );
    }

    group.layout();
    m_creator.updateControlSize( false );

  }
}
