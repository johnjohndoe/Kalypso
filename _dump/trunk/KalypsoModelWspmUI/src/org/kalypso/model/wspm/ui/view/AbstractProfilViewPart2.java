package org.kalypso.model.wspm.ui.view;

import org.eclipse.core.resources.IFile;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.contribs.eclipse.ui.PartAdapter;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilListener;
import org.kalypso.model.wspm.ui.profil.view.IProfilProvider2;
import org.kalypso.model.wspm.ui.profil.view.IProfilProviderListener;
import org.kalypso.model.wspm.ui.profil.view.IProfilViewDataListener;
import org.kalypso.model.wspm.ui.profil.view.ProfilViewData;
import org.kalypso.model.wspm.ui.profil.view.chart.action.ProfilChartViewActionBarContributor;

/**
 * @author belger
 */
public abstract class AbstractProfilViewPart2 extends ViewPart implements IProfilViewPart2, IProfilViewDataListener, IProfilListener, IProfilProviderListener
{
  private final ProfilChartViewActionBarContributor m_actionContributor = new ProfilChartViewActionBarContributor();

  private final IPartListener m_partListener = new PartAdapter()
  {
    /**
     * @see org.kalypso.contribs.eclipse.ui.PartAdapter#partActivated(org.eclipse.ui.IWorkbenchPart)
     */
    @Override
    public void partActivated( final IWorkbenchPart part )
    {
      AbstractProfilViewPart2.this.partActivated( part );
    }

    /**
     * @see org.kalypso.contribs.eclipse.ui.PartAdapter#partClosed(org.eclipse.ui.IWorkbenchPart)
     */
    @Override
    public void partClosed( final IWorkbenchPart part )
    {
      AbstractProfilViewPart2.this.partClosed( part );
    }
  };

  private IProfilEventManager m_pem = null;

  private Composite m_control;

  private ProfilViewData m_viewData = null;

  private IProfilProvider2 m_provider;

  private IWorkbenchPart m_part;

  @Override
  public void init( final IViewSite site ) throws PartInitException
  {
    super.init( site );

    final IWorkbenchPage page = site.getPage();
    page.addPartListener( m_partListener );

    m_actionContributor.init( page );

    final IActionBars actionBars = site.getActionBars();
    m_actionContributor.contributeTo( actionBars.getMenuManager() );
    m_actionContributor.contributeTo( actionBars.getToolBarManager() );
    m_actionContributor.contributeTo( actionBars.getStatusLineManager() );
  }

  protected void setProvider( final IWorkbenchPart part, final IProfilProvider2 provider )
  {
    if( provider == m_provider )
      return;

    if( m_provider != null )
    {
      m_provider.removeProfilProviderListener( this );
      m_provider.dispose();
    }

    m_provider = provider;
    m_part = part;

    if( m_provider != null )
    {
      m_provider.addProfilProviderListener( this );
      onProfilProviderChanged( m_provider, null, provider.getEventManager(), null, provider.getViewData() );
    }

  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#dispose()
   */
  @Override
  public void dispose( )
  {
    getSite().getPage().removePartListener( m_partListener );

    unhookProvider();

    m_actionContributor.dispose();

    super.dispose();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public final void createPartControl( final Composite parent )
  {
    m_control = new Composite( parent, SWT.NONE );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.marginHeight = 0;
    gridLayout.marginWidth = 0;
    m_control.setLayout( gridLayout );

    // find the part which populates this view. The parts are searched in this order:
    // - active part (probaly this object)
    // - active editor
    // - other editors
    // - other views
    final IWorkbenchPage page = getSite().getPage();
    final IWorkbenchPart activePart = page.getActivePart();
    if( partActivated( activePart ) )
      return;

    if( partActivated( page.getActiveEditor() ) )
      return;

    for( final IEditorReference reference : page.getEditorReferences() )
    {
      if( partActivated( reference.getPart( false ) ) )
        return;
    }

    for( final IViewReference reference : page.getViewReferences() )
    {
      if( partActivated( reference.getPart( false ) ) )
        return;
    }

    // if nothing is found, set null profile event manager to init the control
    onProfilProviderChanged( null, null, null, null, null );
  }

  /**
   * Tries to find a IProfileProvider from the given part.
   * 
   * @return true, if a profile provider was found.
   */
  protected boolean partActivated( final IWorkbenchPart part )
  {
    if( part == null )
      return false;

    final IProfilProvider2 provider = (IProfilProvider2) part.getAdapter( IProfilProvider2.class );
    if( provider == null )
      return false;

    setProvider( part, provider );

    return true;
  }

  protected void partClosed( final IWorkbenchPart part )
  {
    if( part == m_part )
    {
      unhookProvider();
      m_part = null;
      onProfilProviderChanged( null, m_pem, null, m_viewData, null );
    }
  }

  /**
   * @see com.bce.profil.ui.view.IProfilProviderListener#onProfilProviderChanged(com.bce.eind.core.profil.IProfilEventManager,
   *      com.bce.eind.core.profil.IProfilEventManager, com.bce.profil.ui.view.ProfilViewData,
   *      com.bce.profil.ui.view.ProfilViewData)
   */
  public void onProfilProviderChanged( final IProfilProvider2 provider, final IProfilEventManager oldPem, final IProfilEventManager newPem, final ProfilViewData oldViewData, final ProfilViewData newViewData )
  {
    unhookProvider();

    setTitleToolTip( "Kein Profil selektiert" );
    setPartName( "Profil Diagrammansicht" );

    m_pem = newPem;
    m_viewData = newViewData;

    if( m_pem != null )
      m_pem.addProfilListener( this );

    if( m_viewData != null )
      m_viewData.addProfilViewDataListener( this );

    onProfilChanged();
  }

  private void unhookProvider( )
  {
    saveState();

    if( m_pem != null )
    {
      m_pem.removeProfilListener( this );
      m_pem = null;
    }

    if( m_viewData != null )
    {
      m_viewData.removeProfilViewDataListener( this );
      m_viewData = null;
    }
  }

  @Override
  public void setFocus( )
  {
    m_control.setFocus();
  }

  /** Recreates the control */
  private final void onProfilChanged( )
  {
    if( m_control != null && !m_control.isDisposed() )
      m_control.getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          final IFile file = m_provider == null ? null : m_provider.getFile();
          if( file != null )
            setTitleToolTip( file.getFullPath().toOSString() );

          if( m_pem != null )
          {
            final IProfil profil = m_pem.getProfil();
            setPartName( "Station km " + profil );
          }

          final Composite parent = getControl();
          if( parent == null || parent.isDisposed() )
            return;

          for( final Control c : parent.getChildren() )
            c.dispose();

          final Control control = createContent( parent );
          if( control != null )
            control.setLayoutData( new GridData( GridData.FILL_BOTH ) );
          parent.layout();
        }
      } );
  }

  protected Composite getControl( )
  {
    return m_control;
  }

  /**
   * @see com.bce.profil.eclipse.view.IProfilViewPart2#getProfilEventManager()
   */
  public IProfilEventManager getProfilEventManager( )
  {
    return m_pem;
  }

  public IFile getFile( )
  {
    return m_provider == null ? null : m_provider.getFile();
  }

  /**
   * @see com.bce.profil.ui.view.IProfilViewDataListener#onProfilViewDataChanged()
   */
  public void onProfilViewDataChanged( )
  {
    // ?
  }

  protected abstract Control createContent( final Composite parent );

  protected abstract void saveState( );

}
