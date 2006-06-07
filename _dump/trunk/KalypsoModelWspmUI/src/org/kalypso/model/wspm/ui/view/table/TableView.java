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
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.operations.UndoRedoActionGroup;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.contribs.eclipse.ui.PartAdapter2;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.editor.IProfilchartEditorListener;
import org.kalypso.model.wspm.ui.editor.ProfilchartEditor;
import org.kalypso.model.wspm.ui.editor.ProfilchartEditorContributor;
import org.kalypso.model.wspm.ui.preferences.PreferenceConstants;
import org.kalypso.model.wspm.ui.profil.operation.ProfilUndoContext;
import org.kalypso.model.wspm.ui.profil.view.IProfilViewProvider;
import org.kalypso.model.wspm.ui.profil.view.table.swt.ProfilSWTTableView;

/**
 * TableView für ein Profil. Ist eine feste View auf genau einem(!) Editor.
 * 
 * @author belger
 */
public class TableView extends ViewPart implements IPropertyChangeListener, IProfilchartEditorListener
{
  private ProfilSWTTableView m_view;

  private ProfilchartEditor m_editor;

  private Composite m_control;

  private UndoRedoActionGroup m_group;

  public ProfilchartEditor getEditor( )
  {
    return m_editor;
  }

  /**
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite)
   */
  @Override
  public void init( final IViewSite site ) throws PartInitException
  {
    super.init( site );

    final IWorkbenchWindow workbenchWindow = site.getWorkbenchWindow();
    if( workbenchWindow != null && workbenchWindow.getActivePage() != null )
    {
      final IEditorPart activeEditor = workbenchWindow.getActivePage().getActiveEditor();
      if( activeEditor instanceof ProfilchartEditor )
      {
        final ProfilchartEditor profilchartEditor = (ProfilchartEditor) activeEditor;
        setEditor( profilchartEditor );
      }
    }

    final IWorkbenchPage myPage = getSite().getPage();
    myPage.addPartListener( new PartAdapter2()
    {
      @Override
      public void partClosed( final IWorkbenchPartReference partRef )
      {
        final IWorkbenchPart part = partRef.getPart( false );
        if( part == TableView.this )
        {
          myPage.removePartListener( this );
        }
        else if( part == getEditor() )
        {
          if( !workbenchWindow.getWorkbench().isClosing() )
            myPage.hideView( TableView.this );
          else
            TableView.this.dispose();
        }
      }
    } );

    KalypsoModelWspmUIPlugin.getDefault().getPreferenceStore().addPropertyChangeListener( this );
  }

  @Override
  public void dispose( )
  {
    super.dispose();

    if( m_group != null )
      m_group.dispose();

    if( m_view != null )
      m_view.dispose();

    if( m_control != null )
      m_control.dispose();

    KalypsoModelWspmUIPlugin.getDefault().getPreferenceStore().removePropertyChangeListener( this );
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

  protected ProfilSWTTableView getView( )
  {
    return m_view;
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
      if( control != null )
        control.setFocus();
    }
  }

  public void setEditor( final ProfilchartEditor editor )
  {
    if( m_editor != null )
      m_editor.removeProfilchartEditorListener( this );

    m_editor = editor;

    if( m_editor != null )
      m_editor.addProfilchartEditorListener( this );

    // register to global actions of editor
    final IActionBars actionBars = getViewSite().getActionBars();
    m_editor.registerCommonGlobalActions( actionBars );
    actionBars.updateActionBars();

    onProfilChanged( m_editor, (IProfil) m_editor.getAdapter( IProfil.class ) );
  }

  /** Gibt zurück, ob diese TableView bereits einem ProfilchartEditor zugeordnet ist oder nicht. */
  public boolean isAttached( )
  {
    return m_editor != null;
  }

  protected void updateControl( )
  {
    final Control[] childcontrols = m_control.getChildren();
    for( final Control c : childcontrols )
      c.dispose();

    if( m_view != null )
      m_view.dispose();

    unregisterGlobalActions();

    final IProfilViewProvider pvp = m_editor == null ? null : (IProfilViewProvider) m_editor.getAdapter( IProfilViewProvider.class );
    if( pvp == null )
    {
      final Label label = new Label( m_control, SWT.BORDER );
      label.setLayoutData( new GridData( GridData.FILL_BOTH ) );
      label.setText( "Kein Profil geladen." );
    }
    else
    {
      m_view = new ProfilSWTTableView( pvp.getProfilEventManager(), pvp.getViewData(), (IFile) m_editor.getAdapter( IFile.class ) );

      registerGlobalActions( m_view );

      final Control control = m_view.createControl( m_control, SWT.NONE );
      control.setLayoutData( new GridData( GridData.FILL_BOTH ) );
      updateAdvanceMode();

      getSite().registerContextMenu( m_view.getContextMenuManager(), m_view.getSelectionProvider() );
      getSite().setSelectionProvider( m_view.getSelectionProvider() );
    }

    m_control.layout();
  }

  public ProfilSWTTableView getTableView( )
  {
    return m_view;
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
   * @see org.eclipse.ui.part.WorkbenchPart#getAdapter(java.lang.Class)
   */
  @Override
  public Object getAdapter( final Class adapter )
  {
    if( m_editor != null )
    {
      final Object a = m_editor.getAdapter( adapter );
      if( a != null )
        return a;
    }

    return super.getAdapter( adapter );
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

  /**
   * @see com.bce.profil.eclipse.editor.IProfilchartEditorListener#onProfilChanged(com.bce.profil.eclipse.editor.ProfilchartEditor,
   *      com.bce.profil.profilinterface.IProfil)
   */
  public void onProfilChanged( final ProfilchartEditor editor, final IProfil newprofil )
  {
    if( m_group != null )
    {
      m_group.dispose();
      m_group = null;
    }

    m_group = new UndoRedoActionGroup( editor.getSite(), new ProfilUndoContext( editor.getProfil() ), true );
    final IActionBars actionBars = getViewSite().getActionBars();
    m_group.fillActionBars( actionBars );

    if( m_control != null && !m_control.isDisposed() )
    {
      m_control.getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          actionBars.updateActionBars();
          updatePartNameAndControl( editor );
        }
      } );
    }
  }

  /** Must be called in the swt thread */
  protected void updatePartNameAndControl( final ProfilchartEditor editor )
  {
    setPartName( editor.getPartName() );
    if( !m_control.isDisposed() ) // control may have been disposed in the meantime
      updateControl();
  }
}
