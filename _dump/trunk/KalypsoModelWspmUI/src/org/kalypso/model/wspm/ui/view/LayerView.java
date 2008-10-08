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

import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.part.ViewPart;
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
import de.openali.odysseus.chart.framework.model.event.impl.AbstractLayerManagerEventListener;
import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILayerManager;
import de.openali.odysseus.chart.framework.view.impl.ChartComposite;

/**
 * @author Gernot Belger
 */
@SuppressWarnings("unchecked")
public class LayerView extends ViewPart implements IAdapterEater, IProfilChartViewProviderListener

{

  private ScrolledForm m_form = null;

  private FormToolkit m_toolkit;

  private final AdapterPartListener m_providerListener = new AdapterPartListener( IProfilChartViewProvider.class, this, EditorFirstAdapterFinder.instance(), EditorFirstAdapterFinder.instance() );

  private IProfilChartViewProvider m_provider;

  private ILayerManager m_layerManager = null;

  private ILayerManagerEventListener m_layerListener = new AbstractLayerManagerEventListener()
  {
    @Override
    public void onActivLayerChanged( IChartLayer layer )
    {
      if( layer.isActive() )
        updatePanel( layer );
    }
  };

  private IChartLayer m_activeLayer;

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
      m_layerManager.removeListener( m_layerListener );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    if( m_form != null && m_form.getBody() != null )
      m_form.getBody().setFocus();
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    m_toolkit = new FormToolkit( parent.getDisplay() );
    m_form = m_toolkit.createScrolledForm( parent );
    m_toolkit.decorateFormHeading( m_form.getForm() );
    m_form.getForm().setMessage( Messages.TableView_9, IMessageProvider.INFORMATION );
    final GridLayout bodyLayout = new GridLayout();
    bodyLayout.marginHeight = 0;
    bodyLayout.marginWidth = 0;
    m_form.getForm().getBody().setLayout( bodyLayout );
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

  final private IChartLayer getActiveLayer( )
  {

    final IChartLayer[] layers = m_layerManager == null ? new IChartLayer[] {} : m_layerManager.getLayers();
    for( final IChartLayer layer : layers )
      if( layer.isActive() )
      {
        return layer;
      }
    return null;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartViewProviderListener#onProfilChartViewChanged(org.kalypso.model.wspm.ui.view.chart.ProfilChartView)
   */
  public void onProfilChartViewChanged( ProfilChartView newProfilChartView )
  {
    final ChartComposite chart = newProfilChartView == null ? null : newProfilChartView.getChart();
    final IChartModel model = chart == null ? null : chart.getChartModel();
    m_layerManager = model == null ? null : model.getLayerManager();
    if( m_layerManager != null )
      m_layerManager.addListener( m_layerListener );

    updatePanel( getActiveLayer() );
  }

  final void updatePanel( final IChartLayer activeLayer )
  {

    if( m_activeLayer == activeLayer )
      return;

    if( m_form == null || m_form.isDisposed() || m_form.getBody() == null )
      return;
    for( final Control ctrl : m_form.getBody().getChildren() )
    {
      if( !ctrl.isDisposed() )
        ctrl.dispose();
    }
    if( activeLayer != null )
    {
      m_form.getForm().setMessage( "", 0 );
      m_form.getForm().setText( activeLayer.getTitle() );
      final IProfilView panel = activeLayer instanceof IProfilChartLayer ? ((IProfilChartLayer) activeLayer).createLayerPanel() : null;

      if( panel != null )
      {
        final Control control = panel.createControl( m_form.getForm().getBody(), m_toolkit );

        control.setLayoutData( new GridData( GridData.FILL_BOTH ) );
      }

      m_activeLayer = activeLayer;
    }
    else
    {
      m_form.getForm().setText( "" );
      m_form.getForm().setMessage( Messages.TableView_9, IMessageProvider.INFORMATION );
    }

    m_form.getForm().layout();

  }
}
