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
package org.kalypso.model.wspm.ui.view.table;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.contribs.eclipse.ui.partlistener.AdapterPartListener;
import org.kalypso.contribs.eclipse.ui.partlistener.EditorFirstAdapterFinder;
import org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilListener;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIExtensions;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.model.wspm.ui.editor.ProfilchartEditor;
import org.kalypso.model.wspm.ui.profil.IProfilProvider2;
import org.kalypso.model.wspm.ui.profil.IProfilProviderListener;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.om.table.TupleResultCellModifier;
import org.kalypso.ogc.gml.om.table.TupleResultContentProvider;
import org.kalypso.ogc.gml.om.table.TupleResultLabelProvider;
import org.kalypso.ogc.gml.om.table.command.ITupleResultViewerProvider;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandlerProvider;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypsodeegree.model.feature.Feature;

/**
 * TableView für ein Profil. Ist eine feste View auf genau einem Profil.
 * 
 * @author Gernot Belger
 * @author kimwerner
 */
public class TableView extends ViewPart implements IAdapterEater<IProfilProvider2>, IProfilProviderListener, ITupleResultViewerProvider
{
  @SuppressWarnings("unchecked")
  private final AdapterPartListener<IProfilProvider2> m_profilProviderListener = new AdapterPartListener<IProfilProvider2>( IProfilProvider2.class, this, EditorFirstAdapterFinder.instance(), EditorFirstAdapterFinder.instance() );

  protected Form m_form;

  private FormToolkit m_toolkit;

  private IProfilProvider2 m_provider;

  protected IProfil m_profile;

  protected DefaultTableViewer m_view;

  private TupleResultContentProvider m_tupleResultContentProvider;

  private TupleResultLabelProvider m_tupleResultLabelProvider;

  protected ProfileProblemView m_problemView = null;

  protected final UIJob m_markerRefreshJob = new UIJob( Messages.TableView_0 )
  {
    @Override
    public IStatus runInUIThread( final IProgressMonitor monitor )
    {
      final IRecord[] points = m_profile.getPoints();
      if( points.length > 0 )
        m_view.update( points, new String[] { "" } );
      m_problemView.updateSections( m_profile );
      return Status.OK_STATUS;
    }
  };

  protected final UIJob m_setActivePointJob = new UIJob( Messages.TableView_0 )
  {
    @Override
    public IStatus runInUIThread( IProgressMonitor monitor )
    {
      final IRecord activePoint = m_profile.getActivePoint();
      m_view.setSelection( new StructuredSelection( activePoint ) );
      m_view.reveal( activePoint );
      return Status.OK_STATUS;
    }
  };

  // TODO: consider moving this in the contentprovider: to do this, extends the TupleResultContentProvider to a
  // ProfileContentProvider
  private final IProfilListener m_profileListener = new IProfilListener()
  {
    public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
    {
      if( hint.isActivePointChanged() )
      {
        m_setActivePointJob.cancel();
        m_setActivePointJob.schedule( 100 );
      }
    }

    /**
     * @see org.kalypso.model.wspm.core.profil.IProfilListener#onProblemMarkerChanged(org.kalypso.model.wspm.core.profil.IProfil)
     */
    public void onProblemMarkerChanged( IProfil source )
    {
      m_markerRefreshJob.cancel();
      m_markerRefreshJob.schedule( 500 );
    }
  };

  private MenuManager m_menuManager;

  /**
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite)
   */
  @Override
  public void init( final IViewSite site ) throws PartInitException
  {
    super.init( site );

    m_profilProviderListener.init( site.getPage() );

    m_menuManager = new MenuManager();
    m_menuManager.add( new GroupMarker( IWorkbenchActionConstants.MB_ADDITIONS ) );
  }

  @Override
  public void dispose( )
  {
    super.dispose();

    if( m_profile != null )
      m_profile.removeProfilListener( m_profileListener );

    m_menuManager.dispose();
    m_menuManager = null;

    unhookProvider();

    if( m_tupleResultContentProvider != null )
      m_tupleResultContentProvider.dispose();

    if( m_tupleResultLabelProvider != null )
      m_tupleResultLabelProvider.dispose();

    m_profilProviderListener.dispose();

    if( m_form != null )
      m_form.dispose();
  }

  private void unhookProvider( )
  {
    if( m_provider != null )
    {
      m_provider.removeProfilProviderListener( this );
      m_provider = null;
    }
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    final IContextService contextService = (IContextService) getSite().getService( IContextService.class );
    if( contextService != null )
      contextService.activateContext( "org.kalypso.model.wspm.ui.view.table.swt.context" ); //$NON-NLS-1$

    m_toolkit = new FormToolkit( parent.getDisplay() );
    m_form = m_toolkit.createForm( parent );
    m_toolkit.decorateFormHeading( m_form );

    final GridLayout bodyLayout = new GridLayout();
    bodyLayout.marginHeight = 0;
    bodyLayout.marginWidth = 0;
    m_form.getBody().setLayout( bodyLayout );

    m_view = new DefaultTableViewer( m_form.getBody(), SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION );
    m_view.getTable().setHeaderVisible( true );
    m_view.getTable().setLinesVisible( true );
    m_view.getTable().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    getSite().setSelectionProvider( m_view );
    getSite().registerContextMenu( m_menuManager, m_view );
    m_view.getTable().setMenu( m_menuManager.createContextMenu( m_view.getTable() ) );

    m_view.addPostSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        if( !selection.isEmpty() )
        {
          final IRecord point = (IRecord) selection.getFirstElement();
          if( point != m_profile.getActivePoint() )
            m_profile.setActivePoint( point );
        }
      }
    } );
    m_problemView = new ProfileProblemView( m_toolkit, m_form );

    updateControl();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    if( m_view != null )
    {
      final Control control = m_view.getControl();
      if( control != null && !control.isDisposed() )
        control.setFocus();
    }
  }

  protected void updateControl( )
  {
    if( m_form == null || m_form.isDisposed() )
      return;

    final ProfilViewData pvd = m_provider == null ? null : m_provider.getViewData();

    if( m_profile == null || pvd == null )
    {
      m_form.setMessage( Messages.TableView_9, IMessageProvider.INFORMATION );
      final GridData tableGrid = (GridData) m_view.getTable().getLayoutData();
      tableGrid.exclude = true;
      m_view.getTable().setVisible( false );

      return;
    }

    /* Create handlers for this profile */
    setContentDescription( "" ); //$NON-NLS-1$
    final GridData tableGrid = (GridData) m_view.getTable().getLayoutData();
    tableGrid.exclude = false;
    m_view.getTable().setVisible( true );

    final IProfilLayerProvider layerProvider = KalypsoModelWspmUIExtensions.createProfilLayerProvider( m_profile.getType() );
    final IComponentUiHandlerProvider handlerProvider = layerProvider.getComponentUiHandlerProvider( m_profile );
    if( m_view.getContentProvider() != null )
      m_view.setInput( null ); // Reset input in order to avoid double refresh

    m_tupleResultContentProvider = new TupleResultContentProvider( handlerProvider );
    m_tupleResultLabelProvider = new TupleResultLabelProvider( m_tupleResultContentProvider );

    m_view.setContentProvider( m_tupleResultContentProvider );
    m_view.setLabelProvider( m_tupleResultLabelProvider );
    m_view.setCellModifier( new TupleResultCellModifier( m_tupleResultContentProvider ) );

    final Feature[] obsFeatures = FeatureSelectionHelper.getAllFeaturesOfType( KalypsoCorePlugin.getDefault().getSelectionManager(), ObservationFeatureFactory.OM_OBSERVATION );
    if( obsFeatures.length > 0 )
      m_view.setInput( m_profile.getResult() );
    else
      m_view.setInput( null );
    m_form.setMessage( null );
    m_view.getControl().getParent().layout();
  }

  /** Must be called in the swt thread */
  protected void updatePartNameAndControl( final ProfilchartEditor editor )
  {
    setPartName( editor.getPartName() );
    if( !m_form.isDisposed() ) // control may have been disposed in the meantime
      updateControl();
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater#setAdapter(java.lang.Object)
   */
  public void setAdapter( final IWorkbenchPart part, final IProfilProvider2 provider )
  {
    if( m_provider == provider && provider != null )
      return;

    unhookProvider();

    m_provider = provider;

    if( m_provider != null )
      m_provider.addProfilProviderListener( this );

    final ProfilViewData viewData = m_provider == null ? null : m_provider.getViewData();
    final IProfil newProfile = m_provider == null ? null : m_provider.getProfil();
    onProfilProviderChanged( m_provider, m_profile, newProfile, null, viewData );
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.IProfilProviderListener#onProfilProviderChanged(org.kalypso.model.wspm.ui.profil.view.IProfilProvider2,
   *      org.kalypso.model.wspm.core.profil.IProfilEventManager,
   *      org.kalypso.model.wspm.core.profil.IProfilEventManager, org.kalypso.model.wspm.ui.profil.view.ProfilViewData,
   *      org.kalypso.model.wspm.ui.profil.view.ProfilViewData)
   */
  public void onProfilProviderChanged( final IProfilProvider2 provider, final IProfil oldProfile, final IProfil newProfile, final ProfilViewData oldViewData, final ProfilViewData newViewData )
  {
    if( m_profile != null )
      m_profile.removeProfilListener( m_profileListener );

    m_profile = newProfile;

    if( m_profile != null )
      m_profile.addProfilListener( m_profileListener );

    if( m_form != null && !m_form.isDisposed() )
      m_form.getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          updateControl();
          m_problemView.updateSections( m_profile );
        }
      } );

  }

  public IProfil getProfil( )
  {
    return m_profile;
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#getAdapter(java.lang.Class)
   */
  @SuppressWarnings("unchecked")
  @Override
  public Object getAdapter( final Class adapter )
  {
    if( adapter == ITupleResultViewerProvider.class )
      return this;

    return super.getAdapter( adapter );
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.command.ITupleResultViewerProvider#getTupleResult()
   */
  public TupleResult getTupleResult( )
  {
    if( m_view == null )
      return null;

    return (TupleResult) m_view.getInput();
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.command.ITupleResultViewerProvider#getTupleResultViewer()
   */
  public TableViewer getTupleResultViewer( )
  {
    return m_view;
  }
}
