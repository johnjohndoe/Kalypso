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

import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.chart.ui.IChartPart;
import org.kalypso.chart.ui.editor.ChartEditorTreeOutlinePage;
import org.kalypso.contribs.eclipse.jface.viewers.SelectionProviderDelegator;
import org.kalypso.contribs.eclipse.ui.partlistener.AdapterPartListener;
import org.kalypso.contribs.eclipse.ui.partlistener.EditorFirstAdapterFinder;
import org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartViewProvider;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartViewProviderListener;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;

import de.openali.odysseus.chart.framework.model.event.ILayerManagerEventListener;
import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.view.impl.ChartComposite;

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
public class LegendView extends ViewPart implements IAdapterEater, ILayerManagerEventListener// ,
                                                                                                   // IActiveLayerProvider
{
  /**
   * @see de.openali.odysseus.chart.framework.model.event.ILayerManagerEventListener#onLayerAdded(de.openali.odysseus.chart.framework.model.layer.IChartLayer)
   */
  @Override
  public void onLayerAdded( IChartLayer layer )
  {
    // TODO Auto-generated method stub
    
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
    // TODO Auto-generated method stub
    
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.event.ILayerManagerEventListener#onLayerVisibilityChanged(de.openali.odysseus.chart.framework.model.layer.IChartLayer)
   */
  @Override
  public void onLayerVisibilityChanged( IChartLayer layer )
  {
    // TODO Auto-generated method stub
    
  }

  private final AdapterPartListener m_chartProviderListener = new AdapterPartListener( IProfilChartViewProvider.class, this, EditorFirstAdapterFinder.instance(), EditorFirstAdapterFinder.instance() );

  // private ChartLegend m_chartlegend;
  private ChartEditorTreeOutlinePage m_chartlegend;

  private Composite m_composite;

  private IProfilChartViewProvider m_provider;

  private final SelectionProviderDelegator m_selectionProviderDelegator = new SelectionProviderDelegator();

  private final MenuManager m_menuMgr = new MenuManager( Messages.LegendView_0 );

  // private final List<IActiveLayerChangeListener> m_layerListener = new ArrayList<IActiveLayerChangeListener>( 5 );

  @Override
  public void init( final IViewSite site ) throws PartInitException
  {
    super.init( site );

    m_chartProviderListener.init( site.getPage() );

    site.setSelectionProvider( m_selectionProviderDelegator );

    // menuMgr.setRemoveAllWhenShown( true );
    m_menuMgr.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( final IMenuManager manager )
      {
        LegendView.this.fillContextMenu( manager );
      }
    } );

    getSite().registerContextMenu( m_menuMgr, m_selectionProviderDelegator );

    // inform active layer listeners of selection changes
    m_selectionProviderDelegator.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        fireOnActiveLayerChanged();
      }
    } );
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#dispose()
   */
  @Override
  public void dispose( )
  {
    m_chartProviderListener.dispose();
    m_selectionProviderDelegator.dispose();

    unhookProvider();
    unhookLegend();

    getSite().setSelectionProvider( null );

    super.dispose();
  }

  private void unhookLegend( )
  {
    if( m_chartlegend != null )
    {
      m_chartlegend.dispose();
      m_chartlegend = null;
    }

    m_selectionProviderDelegator.setDelegate( null );
  }

  private void unhookProvider( )
  {
    if( m_provider != null )
    {
    //  m_provider.removeProfilChartViewProviderListener( this );
      m_provider = null;
    }
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    m_composite = new Composite( parent, SWT.NONE );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.marginHeight = 0;
    gridLayout.marginWidth = 0;
    m_composite.setLayout( gridLayout );
    updateChartLegend();
  }

  private void updateChartLegend( )
  {
    if( m_composite != null && !m_composite.isDisposed() )
    {
      for( final Control control : m_composite.getChildren() )
        control.dispose();

      createContent( m_composite );
      m_composite.layout();
    }

    fireOnActiveLayerChanged();
  }

  private Control createContent( final Composite parent )
  {
    // TODO: remember selected layer names
    unhookLegend();

    final ProfilChartView chartView = getProfilChartView();

    if( chartView == null )
    {
      final Label label = new Label( parent, SWT.CENTER );
      final GridData gridData = new GridData();
      gridData.grabExcessHorizontalSpace = true;
      gridData.horizontalAlignment = SWT.FILL;
      gridData.horizontalIndent = 10;
      gridData.grabExcessVerticalSpace = true;
      gridData.verticalAlignment = SWT.CENTER;
      gridData.verticalIndent = 10;
      label.setLayoutData( gridData );
      label.setText( Messages.LegendView_1 );
      return label;
    }
    else
    {
      // m_chartlegend = new ChartLegend( parent, SWT.BORDER, chartView.getChart(), false );
      // TODO: restore selection

      m_chartlegend = new ChartEditorTreeOutlinePage((IChartPart)getProfilChartView() ); // this

      m_chartlegend.createControl( parent );
// final TreeViewer tv = m_chartlegend.getTreeViewer();
//
// tv.setLabelProvider( new ChartTreeLabelProvider( this )
// {
//
// /**
// * @see org.kalypso.chart.ui.editor.ChartTreeLabelProvider#getText(java.lang.Object)
// */
// @Override
// public String getText( Object element )
// {
// if( element instanceof IProfilChartLayer )
// {
// return ((IProfilChartLayer) element).getTitle();
// }
// return super.getText( element );
// }
// } );
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

      m_selectionProviderDelegator.setDelegate( m_chartlegend.getTreeViewer() );

      // reset menu manager
      m_menuMgr.dispose();
      m_menuMgr.removeAll();
      final Menu menu = m_menuMgr.createContextMenu( control );
      control.setMenu( menu );

      return control;
    }
  }

  public ProfilChartView getProfilChartView( )
  {
    final ProfilChartView chartView = (m_provider == null ? null : m_provider.getProfilChartView());
    return chartView;
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

  public ISelectionProvider getSelectionProvider( )
  {
    return m_selectionProviderDelegator;
  }

  /**
   * Contributes actions to the pop-up menu.
   */
  void fillContextMenu( final IMenuManager menu )
  {
    // update enabled state for actions that aren't updated in selectionChanged
    // final IStructuredSelection selection = (IStructuredSelection)getSelection();
    // resolveMarkerAction.setEnabled( resolveMarkerAction.shouldEnable( selection ) );

    // add the actions to the menu
    // menu.add( removeTaskAction );
    // menu.add( new Separator() );
    menu.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
    menu.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS + "-end" ) ); //$NON-NLS-1$
    // menu.add( propertiesAction );
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    if( m_chartlegend != null )
    {
      final Control control = m_chartlegend.getControl();
      if( control != null && !control.isDisposed() )
        control.setFocus();
    }
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

    if( m_provider != null )
    {
     // m_provider.addProfilChartViewProviderListener( this );
      m_provider.getProfilChartView().getChart().getChartModel().getLayerManager().addListener( this );
    }

    updateChartLegend();
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.IProfilChartViewProviderListener#ProfilChartViewChanged(org.kalypso.model.wspm.ui.profil.view.chart.ProfilChartView)
   */
  public void onProfilChartViewChanged( final ProfilChartView newProfilChartView )
  {
    updateChartLegend();
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#getAdapter(java.lang.Class)
   */

  @SuppressWarnings("unchecked")
  @Override
  public Object getAdapter( final Class adapter )
  {
    if( adapter == IProfilChartViewProviderListener.class )
      return this;

    return super.getAdapter( adapter );
  }

  /**
   * @see de.belger.swtchart.layer.IActiveLayerProvider#getActiveLayer()
   */
  public IChartLayer getActiveLayer( )
  {

    final ISelection selection = getSelectionProvider().getSelection();
    if( !(selection instanceof IStructuredSelection) )
      return null;

    final IStructuredSelection structSel = (IStructuredSelection) selection;
    final Object firstElement = structSel.getFirstElement();

    if( firstElement instanceof IChartLayer )
      return (IChartLayer) firstElement;

    return null;
  }

// /**
// * @see de.belger.swtchart.layer.IActiveLayerProvider#addActiveLayerChangeListener(de.belger.swtchart.layer.
  // IActiveLayerChangeListener)
// */
// public void addActiveLayerChangeListener( final IActiveLayerChangeListener l )
// {
// m_layerListener.add( l );
// }
//
// /**
// * @see de.belger.swtchart.layer.IActiveLayerProvider#removeActiveLayerChangeListener(de.belger.swtchart.layer.
  // IActiveLayerChangeListener)
// */
// public void removeActiveLayerChangeListener( final IActiveLayerChangeListener l )
// {
// m_layerListener.add( l );
// }
//
  protected void fireOnActiveLayerChanged( )
  {

    final ProfilChartView view = getProfilChartView();
    final ChartComposite chart = view == null ? null :  view.getChart();

    if( chart == null )
      return;
    final IChartLayer activeLayer = getActiveLayer();
    for( final IChartLayer layer : chart.getChartModel().getLayerManager().getLayers() )
    {
      layer.setActive( activeLayer == layer );
// if( layer.isActive() )
// {
// for( final IActiveLayerChangeListener l : m_layerListener )
// l.onActiveLayerChanged( layer );
// return;
// }
    }
  }
}
