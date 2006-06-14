package org.kalypso.model.wspm.ui.view.table;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.operations.UndoRedoActionGroup;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.contribs.eclipse.ui.partlistener.AdapterPartListener;
import org.kalypso.contribs.eclipse.ui.partlistener.EditorFirstAdapterFinder;
import org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.editor.ProfilchartEditor;
import org.kalypso.model.wspm.ui.editor.ProfilchartEditorContributor;
import org.kalypso.model.wspm.ui.preferences.PreferenceConstants;
import org.kalypso.model.wspm.ui.profil.view.IProfilProvider2;
import org.kalypso.model.wspm.ui.profil.view.IProfilProviderListener;
import org.kalypso.model.wspm.ui.profil.view.ProfilViewData;
import org.kalypso.model.wspm.ui.profil.view.table.swt.ProfilSWTTableView;

/**
 * TableView für ein Profil. Ist eine feste View auf genau einem(!) Editor.
 * 
 * @author belger
 */
public class TableView extends ViewPart implements IPropertyChangeListener, IAdapterEater, IProfilProviderListener
{
  private final AdapterPartListener m_profilProviderListener = new AdapterPartListener( IProfilProvider2.class, this, EditorFirstAdapterFinder.instance(), EditorFirstAdapterFinder.instance() );

  private ProfilSWTTableView m_view;

  private Composite m_control;

  private UndoRedoActionGroup m_group;

  private IProfilProvider2 m_provider;

  private IProfilEventManager m_pem;

  /**
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite)
   */
  @Override
  public void init( final IViewSite site ) throws PartInitException
  {
    super.init( site );

    m_profilProviderListener.init( site.getPage() );

    KalypsoModelWspmUIPlugin.getDefault().getPreferenceStore().addPropertyChangeListener( this );
  }

  @Override
  public void dispose( )
  {
    super.dispose();

    unhookProvider();

    m_profilProviderListener.dispose();

    if( m_group != null )
      m_group.dispose();

    if( m_view != null )
      m_view.dispose();

    if( m_control != null )
      m_control.dispose();

    KalypsoModelWspmUIPlugin.getDefault().getPreferenceStore().removePropertyChangeListener( this );
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
    m_control = new Composite( parent, SWT.NONE );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.marginHeight = 0;
    gridLayout.marginWidth = 0;
    m_control.setLayout( gridLayout );

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
      if( control != null && !m_control.isDisposed() )
        control.setFocus();
    }
  }

  protected void updateControl( )
  {
    if( m_control == null || m_control.isDisposed() )
      return;

    final Control[] childcontrols = m_control.getChildren();
    for( final Control c : childcontrols )
      c.dispose();

    if( m_view != null )
      m_view.dispose();

    unregisterGlobalActions();

    final IProfilEventManager pem = m_provider == null ? null : m_provider.getEventManager();
    final ProfilViewData pvd = m_provider == null ? null : m_provider.getViewData();

    if( pem == null || pvd == null )
    {
      final Label label = new Label( m_control, SWT.BORDER );
      label.setLayoutData( new GridData( GridData.FILL_BOTH ) );
      label.setText( "Kein Profil geladen." );
    }
    else
    {
      final IFile file = (IFile) m_profilProviderListener.getPart().getAdapter( IFile.class );

      m_view = new ProfilSWTTableView( pem, pvd, file );

      registerGlobalActions( m_view );

      final Control control = m_view.createControl( m_control, SWT.NONE );
      control.setLayoutData( new GridData( GridData.FILL_BOTH ) );
      updateAdvanceMode();

      getSite().registerContextMenu( m_view.getContextMenuManager(), m_view.getSelectionProvider() );
      getSite().setSelectionProvider( m_view.getSelectionProvider() );
    }

    m_control.layout();
  }

  private void registerGlobalActions( final ProfilSWTTableView tableView )
  {
    final IActionBars actionBars = getViewSite().getActionBars();

    actionBars.setGlobalActionHandler( ActionFactory.COPY.getId(), tableView.getAction( ProfilSWTTableView.ACTION_COPY ) );
    actionBars.setGlobalActionHandler( ActionFactory.PASTE.getId(), tableView.getAction( ProfilSWTTableView.ACTION_PASTE ) );
    actionBars.setGlobalActionHandler( ActionFactory.DELETE.getId(), tableView.getAction( ProfilSWTTableView.ACTION_DELETEPOINTS ) );
    actionBars.setGlobalActionHandler( ActionFactory.SELECT_ALL.getId(), tableView.getAction( ProfilSWTTableView.ACTION_SELECTALL ) );
    actionBars.setGlobalActionHandler( ProfilchartEditorContributor.RETARGET_INSERT, tableView.getAction( ProfilSWTTableView.ACTION_INSERTPOINT ) );

    actionBars.updateActionBars();
  }

  private void unregisterGlobalActions( )
  {
    final IActionBars actionBars = getViewSite().getActionBars();

    actionBars.setGlobalActionHandler( ActionFactory.COPY.getId(), null );
    actionBars.setGlobalActionHandler( ActionFactory.PASTE.getId(), null );
    actionBars.setGlobalActionHandler( ActionFactory.DELETE.getId(), null );
    actionBars.setGlobalActionHandler( ActionFactory.SELECT_ALL.getId(), null );
    actionBars.setGlobalActionHandler( ProfilchartEditorContributor.RETARGET_INSERT, null );

    actionBars.updateActionBars();
  }

  /**
   * @see org.eclipse.jface.util.IPropertyChangeListener#propertyChange(org.eclipse.jface.util.PropertyChangeEvent)
   */
  public void propertyChange( final PropertyChangeEvent event )
  {
    if( PreferenceConstants.P_TABLE_ADVANCE_MODE.equals( event.getProperty() ) )
      updateAdvanceMode();
  }

  private void updateAdvanceMode( )
  {
    m_view.setAdvanceMode( KalypsoModelWspmUIPlugin.getDefault().getPreferenceStore().getString( PreferenceConstants.P_TABLE_ADVANCE_MODE ) );
  }

  public static String[][] getAdvanceModes( )
  {
    return ProfilSWTTableView.getAdvanceModes();
  }

  /** Must be called in the swt thread */
  protected void updatePartNameAndControl( final ProfilchartEditor editor )
  {
    setPartName( editor.getPartName() );
    if( !m_control.isDisposed() ) // control may have been disposed in the meantime
      updateControl();
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater#setAdapter(java.lang.Object)
   */
  public void setAdapter( final Object adapter )
  {
    final IProfilProvider2 provider = (IProfilProvider2) adapter;

    if( m_provider == provider && provider != null )
      return;

    unhookProvider();

    m_provider = provider;

    if( m_provider != null )
      m_provider.addProfilProviderListener( this );

    final IProfilEventManager pem = m_provider == null ? null : m_provider.getEventManager();
    final ProfilViewData viewData = m_provider == null ? null : m_provider.getViewData();
    onProfilProviderChanged( m_provider, null, pem, null, viewData );
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.IProfilProviderListener#onProfilProviderChanged(org.kalypso.model.wspm.ui.profil.view.IProfilProvider2,
   *      org.kalypso.model.wspm.core.profil.IProfilEventManager,
   *      org.kalypso.model.wspm.core.profil.IProfilEventManager, org.kalypso.model.wspm.ui.profil.view.ProfilViewData,
   *      org.kalypso.model.wspm.ui.profil.view.ProfilViewData)
   */
  public void onProfilProviderChanged( final IProfilProvider2 provider, final IProfilEventManager oldPem, final IProfilEventManager newPem, final ProfilViewData oldViewData, ProfilViewData newViewData )
  {
    // if( m_group != null )
    // {
    // m_group.dispose();
    // m_group = null;
    // }
    //
    // m_group = new UndoRedoActionGroup( getSite(), new ProfilUndoContext( getProfil() ), true );
    // final IActionBars actionBars = getViewSite().getActionBars();
    // m_group.fillActionBars( actionBars );
    //
    // if( m_control != null && !m_control.isDisposed() )
    // {
    // m_control.getDisplay().asyncExec( new Runnable()
    // {
    // public void run( )
    // {
    // actionBars.updateActionBars();
    // updatePartNameAndControl( editor );
    // }
    // } );
    // }

    m_pem = newPem;

    if( m_control != null && !m_control.isDisposed() )
    {
      m_control.getDisplay().syncExec( new Runnable()
      {
        public void run( )
        {
          updateControl();
        }
      } );
    }

  }

  public IProfilEventManager getProfilEventManager( )
  {
    return m_pem;
  }

  public ProfilSWTTableView getTableView( )
  {
    return m_view;
  }
}
