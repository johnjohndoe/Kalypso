/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import java.awt.Color;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.JFaceResources;
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
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.internal.ide.IDEInternalWorkbenchImages;
import org.eclipse.ui.operations.UndoRedoActionGroup;
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
import org.kalypso.model.wspm.core.profil.MarkerIndex;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIExtensions;
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
 * TableView f�r ein Profil. Ist eine feste View auf genau einem Profil.
 * 
 * @author Gernot Belger
 * @author kimwerner
 */
public class TableView extends ViewPart implements IAdapterEater<IProfilProvider2>, IProfilProviderListener, ITupleResultViewerProvider
{
  private final AdapterPartListener<IProfilProvider2> m_profilProviderListener = new AdapterPartListener<IProfilProvider2>( IProfilProvider2.class, this, EditorFirstAdapterFinder.instance(), EditorFirstAdapterFinder.instance() );

  protected Form m_form;

 // private UndoRedoActionGroup m_group;

  private FormToolkit m_toolkit;

  private IProfilProvider2 m_provider;

  protected IProfil m_profile;

  private DefaultTableViewer m_view;

  private TupleResultContentProvider m_tupleResultContentProvider;

  private TupleResultLabelProvider m_tupleResultLabelProvider;

  private final class MarkerAction extends Action
  {
    final IMarker m_marker;

    public MarkerAction( final IMarker marker )
    {
      super();
      m_marker = marker;
    }

    /**
     * @see org.eclipse.jface.action.Action#getImageDescriptor()
     */
    @SuppressWarnings("restriction")
    @Override
    public ImageDescriptor getImageDescriptor( )
    {

      switch( m_marker.getAttribute( IMarker.SEVERITY, 0 ) )
      {
        case IMarker.SEVERITY_ERROR:
          return IDEInternalWorkbenchImages.getImageDescriptor( IDEInternalWorkbenchImages.IMG_OBJS_ERROR_PATH );
        case IMarker.SEVERITY_WARNING:
          return IDEInternalWorkbenchImages.getImageDescriptor( IDEInternalWorkbenchImages.IMG_OBJS_WARNING_PATH );
        case IMarker.SEVERITY_INFO:
          return IDEInternalWorkbenchImages.getImageDescriptor( IDEInternalWorkbenchImages.IMG_OBJS_INFO_PATH );
        default:
          return null;
      }
    }

    /**
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText( )
    {
      return m_marker.getAttribute( IMarker.MESSAGE, "" );
    }

    /**
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run( )
    {
      final int pos = m_marker.getAttribute( IValidatorMarkerCollector.MARKER_ATTRIBUTE_POINTPOS, -1 );
      if( pos > 0 )
      {
        final IProfil profil = getProfil();
        if( profil == null || pos < 0 )
          return;
        profil.setActivePoint( profil.getPoint( pos ) );
      }
    }

  }

  // TODO: consider moving this in the contentprovider: to do this, extends the TupleResultContentProvider to a
  // ProfileContentProvider
  private final IProfilListener m_profileListener = new IProfilListener()
  {
    public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
    {
      if( hint.isActivePointChanged() )
      {
        new UIJob( "updating cross section table..." )
        {
          @Override
          public IStatus runInUIThread( IProgressMonitor monitor )
          {
            final IRecord activePoint = m_profile.getActivePoint();
            m_view.setSelection( new StructuredSelection( activePoint ) );
            m_view.reveal( activePoint );
            return Status.OK_STATUS;
          }
        }.schedule();
      }
    }

    /**
     * @see org.kalypso.model.wspm.core.profil.IProfilListener#onProblemMarkerChanged(org.kalypso.model.wspm.core.profil.IProfil)
     */
    public void onProblemMarkerChanged( IProfil source )
    {
      final IProfil profil = source;
      Display.getDefault().asyncExec( new Runnable()
      {
        public void run( )
        {
          final IMarker[] markers = profil.getProblemMarker().getMarkers();
          final IMarker worst = MarkerUtils.worstOf( markers );

          if( worst == null )
          {
            if( m_form.getHeadClient() != null )
            {
              m_form.getHeadClient().dispose();
              m_form.setHeadClient( null );
            }
            m_form.setMessage( null );

            // m_problemMarkerSection.setParent(m_form.getHead() );

          }
          else
          {
            if( markers.length > 1 )
              createProblemSection( markers );
            else
            {
              if( m_form.getHeadClient() != null )
              {
                m_form.getHeadClient().dispose();
                m_form.setHeadClient( null );
              }
              m_form.setMessage( worst.getAttribute( IMarker.MESSAGE, "" ), worst.getAttribute( IMarker.SEVERITY, IMessageProvider.NONE ) + 1 );
            }
          }

          // m_form.getMenuManager().removeAll();
// for( IMarker marker : markers )
// {
// m_form.getMenuManager().add( new MarkerAction( marker ) );
// }
// }
// else
// {
// m_form.setText( null );
// if( m_form.getHeadClient() != null )
// m_form.getHeadClient().dispose();
// m_form.setHeadClient( null );
// m_problemMarkerSection.setVisible( false );
// m_form.setMessage( worst.getAttribute( IMarker.MESSAGE, "" ), worst.getAttribute( IMarker.SEVERITY,
// IMessageProvider.NONE ) + 1 );
//             

          // }

        }
      } );

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

//    if( m_group != null )
//      m_group.dispose();

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

  private final void createProblemSection( final IMarker[] markers )
  {
    if( m_form.getHeadClient() != null )
    {
      m_form.getHeadClient().dispose();
      m_form.setHeadClient( null );
    }
    m_form.setMessage( null );
    final Section section = m_toolkit.createSection( m_form.getHead(), Section.TITLE_BAR | Section.TWISTIE );
    section.setLayout( new GridLayout() );
    section.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    section.setTitleBarForeground( section.getDisplay().getSystemColor( SWT.COLOR_RED ) );
    section.setToggleColor( section.getDisplay().getSystemColor( SWT.COLOR_RED ) );

    section.setText( markers.length + " Fehler im Profil" );
    final Composite expanded = m_toolkit.createComposite( section );
    expanded.setLayout( new GridLayout( 1, true ) );
    expanded.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    for( final IMarker marker : markers )
    {
      final ImageHyperlink link = m_toolkit.createImageHyperlink( expanded, SWT.WRAP );
      link.addHyperlinkListener( new HyperlinkAdapter()
      {

        /**
         * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
         */
        @Override
        public void linkActivated( HyperlinkEvent e )
        {
          final IProfil profil = getProfil();
          if( profil == null )
            return;

          final int pointPos = marker.getAttribute( IValidatorMarkerCollector.MARKER_ATTRIBUTE_POINTPOS, -1 );
          if( pointPos < 0 )
            return;
          final IRecord record = getProfil().getPoint( pointPos );
          getProfil().setActivePoint( record );
        }
      } );
      link.setText( marker.getAttribute( IMarker.MESSAGE, "" ) );
      link.setImage( JFaceResources.getResources().createImageWithDefault( IDEInternalWorkbenchImages.getImageDescriptor( IDEInternalWorkbenchImages.IMG_OBJS_ERROR_PATH ) ) );
      link.setForeground( section.getDisplay().getSystemColor( SWT.COLOR_RED ) );
    }

    // expanded.layout(true);
//
    section.setClient( expanded );
// section.layout(true);
    // section.setExpanded( true );
    m_form.setHeadClient( section );
    // m_form.getHead().layout(true);

  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    final IContextService contextService = (IContextService) getSite().getService( IContextService.class );
    if( contextService != null )
      contextService.activateContext( "org.kalypso.model.wspm.ui.view.table.swt.context" );

    m_toolkit = new FormToolkit( parent.getDisplay() );
    m_form = m_toolkit.createForm( parent );
    m_toolkit.decorateFormHeading( m_form );
    m_form.addMessageHyperlinkListener( new HyperlinkAdapter()
    {

      /**
       * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
       */
      @Override
      public void linkActivated( HyperlinkEvent e )
      {
        final IProfil profil = getProfil();
        if( profil == null )
          return;
        final MarkerIndex mi = getProfil().getProblemMarker();
        final IMarker[] markers = mi.getMarkers();
        if( markers.length == 0 )
          return;
        final int pointPos = markers[0].getAttribute( IValidatorMarkerCollector.MARKER_ATTRIBUTE_POINTPOS, -1 );
        if( pointPos < 0 )
          return;
        final IRecord record = getProfil().getPoint( pointPos );
        getProfil().setActivePoint( record );
      }
    } );
    final GridLayout bodyLayout = new GridLayout();
    bodyLayout.marginHeight = 0;
    bodyLayout.marginWidth = 0;
    m_form.getBody().setLayout( bodyLayout );

    m_view = new DefaultTableViewer( m_form.getBody(), SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION );
    m_view.getTable().setHeaderVisible( true );
    m_view.getTable().setLinesVisible( true );
// final GridLayout gridLayout = new GridLayout();
// gridLayout.marginHeight = 0;
// gridLayout.marginWidth = 0;
// m_view.getTable().setLayout( gridLayout );
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
      m_form.setMessage( "Kein Profil geladen", IMessageProvider.INFORMATION );
      final GridData tableGrid = (GridData) m_view.getTable().getLayoutData();
      tableGrid.exclude = true;
      m_view.getTable().setVisible( false );

      return;
    }

    /* Create handlers for this profile */
    setContentDescription( "" );
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
    onProfilProviderChanged( m_provider, null, m_profile, null, viewData );
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
