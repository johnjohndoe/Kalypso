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
package org.kalypso.model.wspm.ui.view.legend;

import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.chart.ui.editor.ChartEditorTreeOutlinePage;
import org.kalypso.contribs.eclipse.ui.partlistener.AdapterPartListener;
import org.kalypso.contribs.eclipse.ui.partlistener.EditorFirstAdapterFinder;
import org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartViewProvider;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartViewProviderListener;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;

import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.IExpandableChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILayerManager;

/**
 * This view shows the profile legend. It always shows the legend of the last active part which adapts to
 * {@link org.kalypso.model.wspm.ui.profil.view.chart}.
 * <p>
 * It is also a selection provider of its selected layers.
 * </p>
 * 
 * @author Gernot Belger
 */
@SuppressWarnings("unchecked")
public class LegendView extends ViewPart implements IAdapterEater, IProfilChartViewProviderListener
{

  private final AdapterPartListener m_chartProviderListener = new AdapterPartListener( IProfilChartViewProvider.class, this, EditorFirstAdapterFinder.instance(), EditorFirstAdapterFinder.instance() );

  private Form m_composite;

  private FormToolkit m_toolkit;

  private IProfilChartViewProvider m_provider;

  private void createContent( )
  {
    if( m_composite == null || m_composite.isDisposed() )
      return;
    final ProfilChartView chartView = getProfilChartView();
    if( chartView == null )
    {
      m_composite.setMessage( Messages.TableView_9, IMessageProvider.INFORMATION );
    }
    else
    {
      createChartLegend( m_composite.getBody(), chartView );
      m_composite.layout();
    }
  }

  private final void createChartLegend( final Composite parent, final ProfilChartView chartView )
  {
    final ChartEditorTreeOutlinePage chartlegend = new ChartEditorTreeOutlinePage( chartView );
    chartlegend.createControl( parent );
    chartlegend.addSelectionChangedListener( new ISelectionChangedListener()
    {
      /**
       * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
       */
      public void selectionChanged( SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        final Object firstElement = selection.getFirstElement();
        if( firstElement instanceof IExpandableChartLayer )
        {
          final IChartLayer activeLayer = (IChartLayer) firstElement;
          final ILayerManager mngr = getProfilChartView().getChart().getChartModel().getLayerManager();
          for( final IChartLayer layer : mngr.getLayers() )
          {
            layer.setActive( activeLayer == layer );
          }
        }
      }
    } );

    final Control control = chartlegend.getControl();
    control.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    control.addMouseListener( new MouseAdapter()
    {
      @Override
      public void mouseDoubleClick( final MouseEvent e )
      {
        showLayerProperties();
      }
    } );
    final ILayerManager mngr = chartView.getChart().getChartModel().getLayerManager();
    for( final IChartLayer layer : mngr.getLayers() )
    {
      if( layer.isActive() )
      {
        chartlegend.setSelection( new StructuredSelection( layer ) );
        chartlegend.getTreeViewer().setExpandedElements( new Object[] { layer } );
        break;
      }
    }
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    m_toolkit = new FormToolkit( parent.getDisplay() );
    m_composite = m_toolkit.createForm( parent );
    m_toolkit.decorateFormHeading( m_composite );

    final GridLayout bodyLayout = new GridLayout();
    bodyLayout.marginHeight = 10;
    bodyLayout.marginWidth = 0;
    m_composite.getBody().setLayout( bodyLayout );

    updateChartLegend();
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#dispose()
   */
  @Override
  public void dispose( )
  {
    m_chartProviderListener.dispose();
    unhookProvider();
    getSite().setSelectionProvider( null );

    super.dispose();
  }

  /**
   * Contributes actions to the pop-up menu.
   */

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#getAdapter(java.lang.Class)
   */

  @Override
  public Object getAdapter( final Class adapter )
  {
    if( adapter == ChartEditorTreeOutlinePage.class )
      return m_composite.getBody();// m_chartlegend;

    return super.getAdapter( adapter );
  }

  public ProfilChartView getProfilChartView( )
  {
    final ProfilChartView chartView = (m_provider == null ? null : m_provider.getProfilChartView());
    return chartView;
  }

  @Override
  public void init( final IViewSite site ) throws PartInitException
  {
    super.init( site );

    m_chartProviderListener.init( site.getPage() );

  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.IProfilChartViewProviderListener#ProfilChartViewChanged(org.kalypso.model.wspm.ui.profil.view.chart.ProfilChartView)
   */
  public void onProfilChartViewChanged( final ProfilChartView newProfilChartView )
  {
    updateChartLegend();
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater#setAdapter(java.lang.Object)
   */
  public void setAdapter( final IWorkbenchPart part, final Object adapter )
  {
    final IProfilChartViewProvider provider = (IProfilChartViewProvider) adapter;
    if( m_provider == provider )
      return;

    unhookProvider();

    m_provider = provider;
    m_provider.addProfilChartViewProviderListener( this );

    updateChartLegend();
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    if( m_composite != null )
    {
      final Control control = m_composite.getBody();
      if( control != null && !control.isDisposed() )
        control.setFocus();
    }
  }

  public void showLayerProperties( )
  {
    // just open layer view
    try
    {
      getSite().getPage().showView( "org.kalypso.model.wspm.ui.view.LayerView" ); //$NON-NLS-1$
    }
    catch( final PartInitException ex )
    {
      ErrorDialog.openError( getSite().getShell(), Messages.LegendView_3, Messages.LegendView_4, ex.getStatus() );
      KalypsoModelWspmUIPlugin.getDefault().getLog().log( ex.getStatus() );
    }
  }

  private void unhookProvider( )
  {
    if( m_provider != null )
    {
      m_provider.removeProfilChartViewProviderListener( this );
      m_provider = null;
    }
  }

  private void updateChartLegend( )
  {
    if( m_composite != null && !m_composite.isDisposed() )
    {
      m_composite.setMessage( null );
      for( final Control control : m_composite.getBody().getChildren() )
      {
        if( !control.isDisposed() )
          control.dispose();
      }
    }
    createContent();

  }

}
