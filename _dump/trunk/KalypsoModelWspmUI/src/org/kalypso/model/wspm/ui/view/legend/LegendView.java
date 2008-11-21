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
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.chart.ui.IChartPart;
import org.kalypso.chart.ui.editor.ChartEditorTreeOutlinePage;
import org.kalypso.contribs.eclipse.ui.partlistener.AdapterPartListener;
import org.kalypso.contribs.eclipse.ui.partlistener.EditorFirstAdapterFinder;
import org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.model.wspm.ui.profil.IProfilProvider2;
import org.kalypso.model.wspm.ui.profil.IProfilProviderListener;
import org.kalypso.model.wspm.ui.view.LayerView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;

import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILayerManager;

/**
 * This view shows the profile legend. It always shows the legend of the last active part which adapts to
 * {@link org.kalypso.model.wspm.ui.profil.view.chart}.
 * <p>
 * It is also a selection provider of its selected layers.
 * </p>
 * 
 * @author Gernot Belger
 * @author kimwerner
 */

@SuppressWarnings("unchecked")
public class LegendView extends ViewPart implements IAdapterEater, IProfilProviderListener
{

  private final AdapterPartListener m_chartProviderListener = new AdapterPartListener( IChartPart.class, this, EditorFirstAdapterFinder.instance(), EditorFirstAdapterFinder.instance() );

  private Form m_composite;

  private FormToolkit m_toolkit;

  private ProfilChartView m_chart;

  private ChartEditorTreeOutlinePage m_chartlegend;

  private final void createChartLegend( final Composite parent, final ProfilChartView chartView )
  {
    m_chartlegend = new ChartEditorTreeOutlinePage( chartView );
    m_chartlegend.createControl( parent );

    m_chartlegend.getTreeViewer().setContentProvider( new ProfilChartEditorTreeContentProvider( chartView.getChart().getChartModel() ) );
    m_chartlegend.addSelectionChangedListener( new ISelectionChangedListener()
    {
      /**
       * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
       */
      public void selectionChanged( SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        final Object firstElement = selection.getFirstElement();
        if( firstElement instanceof IChartLayer )
        {
          ((IChartLayer) firstElement).setActive( true );

          final ProfilChartView pcv = getProfilChartView();
          if( pcv == null )
            return;
          final ILayerManager mngr = pcv.getChart().getChartModel().getLayerManager();

          final IViewReference ref = getSite().getPage().findViewReference( "org.kalypso.model.wspm.ui.view.LayerView" );
          final IViewPart view = ref == null ? null : ref.getView( false );
          if( view != null && view instanceof LayerView )
          {
            for( final IChartLayer layer : mngr.getLayers() )
            {
              if( layer.isActive() )
              {
                ((LayerView) view).updatePanel( layer );
                break;
              }
            }
          }
        }
      }
    } );

    final Control control = m_chartlegend.getControl();
    control.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    control.addMouseListener( new MouseAdapter()
    {
      @Override
      public void mouseDoubleClick( final MouseEvent e )
      {
        showLayerProperties();
      }
    } );

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
    m_chart.removeProfilProviderListener( this );
    getSite().setSelectionProvider( null );
    m_chartlegend.dispose();
    m_chartlegend = null;
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
      return m_chartlegend;

    return super.getAdapter( adapter );
  }

  public ProfilChartView getProfilChartView( )
  {
    return m_chart;
  }

  @Override
  public void init( final IViewSite site ) throws PartInitException
  {
    super.init( site );

    m_chartProviderListener.init( site.getPage() );

  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater#setAdapter(java.lang.Object)
   */
  public void setAdapter( final IWorkbenchPart part, final Object adapter )
  {
    if( m_chart == adapter )
      return;
    m_chart = adapter instanceof ProfilChartView ? (ProfilChartView) adapter : null;

    if( m_chart != null )
      m_chart.addProfilProviderListener( this );

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

  /**
   * @see org.kalypso.model.wspm.ui.profil.IProfilProviderListener#onProfilProviderChanged(org.kalypso.model.wspm.ui.profil.IProfilProvider2,
   *      org.kalypso.model.wspm.core.profil.IProfil, org.kalypso.model.wspm.core.profil.IProfil,
   *      org.kalypso.model.wspm.ui.view.ProfilViewData, org.kalypso.model.wspm.ui.view.ProfilViewData)
   */
  @Override
  public void onProfilProviderChanged( IProfilProvider2 provider, IProfil oldProfile, IProfil newProfile, ProfilViewData oldViewData, ProfilViewData newViewData )
  {
    updateChartLegend();

  }

  private void updateChartLegend( )
  {
    if( m_composite == null || m_composite.isDisposed() )
      return;

    final ProfilChartView chartView = getProfilChartView();
    if( chartView == null )
    {
      m_composite.setMessage( Messages.TableView_9, IMessageProvider.INFORMATION );
      if( m_chartlegend != null )
      {
        m_chartlegend.dispose();
        m_chartlegend = null;
      }
    }
    else
    {
      m_composite.setMessage( null );
      if( m_chartlegend == null )
        createChartLegend( m_composite.getBody(), chartView );
      final ILayerManager mngr = chartView.getChart().getChartModel().getLayerManager();
      for( final IChartLayer layer : mngr.getLayers() )
      {
        if( layer.isActive() )
        {
          m_chartlegend.getTreeViewer().setExpandedElements( new Object[] { layer } );
          m_chartlegend.getTreeViewer().expandToLevel( 2 );
          m_chartlegend.setSelection( new StructuredSelection( layer ) );
          break;
        }
      }
      m_chartlegend.getTreeViewer().refresh();
      m_composite.layout();
    }

  }

}
