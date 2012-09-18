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
package org.kalypso.model.wspm.pdb.ui.internal.content;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.events.IHyperlinkListener;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.UIJob;
import org.eclipse.ui.services.IEvaluationService;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.contribs.eclipse.ui.forms.MessageUtilitites;
import org.kalypso.contribs.eclipse.ui.forms.ToolkitUtils;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.PdbSettings;
import org.kalypso.model.wspm.pdb.db.OpenConnectionThreadedOperation;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.PdbUpdater;
import org.kalypso.model.wspm.pdb.ui.internal.IPdbHelp;
import org.kalypso.model.wspm.pdb.ui.internal.IWaterBodyStructure;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiImages;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiImages.IMAGE;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.ui.internal.preferences.OpenConnectionData;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.FindViewRunnable;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.PdbWspmProject;
import org.kalypso.model.wspm.tuhh.ui.light.WspmMapViewPart;

/**
 * @author Gernot Belger
 */
public class PdbView extends ViewPart implements IConnectionViewer
{
  public static final String ID = "PdbView"; //$NON-NLS-1$

  private static final String MEMENTO_AUTOCONNECT = "autoconnect.name"; //$NON-NLS-1$

  private final Action m_disconnectAction = new DisconnectPdbAction( this );

  private final IAction m_infoAction = new InfoPdbAction( this );

  private final UIJob m_updateControlJob = new UIJob( Messages.getString( "PdbView.0" ) ) //$NON-NLS-1$
  {
    @Override
    public IStatus runInUIThread( final IProgressMonitor monitor )
    {
      updateControl();
      return Status.OK_STATUS;
    }
  };

  private final IHyperlinkListener m_statusListener = new HyperlinkAdapter()
  {
    @Override
    public void linkActivated( final HyperlinkEvent e )
    {
      openConnectionStatus();
    }
  };

  private final OpenConnectionData m_autoConnectData = new OpenConnectionData();

  private Form m_form;

  private FormToolkit m_toolkit;

  private IPdbConnection m_pdbConnection;

  private PdbWspmProject m_wspmProject;

  private boolean m_autoConnectWasDone = false;

  private ConnectionViewer m_connectionViewer;

  private IStatus m_connectionStatus;

  public PdbView( )
  {
    m_updateControlJob.setSystem( true );
  }

  @Override
  public void init( final IViewSite site, final IMemento memento ) throws PartInitException
  {
    super.init( site, memento );

    if( memento != null )
    {
      final String autoConnectName = memento.getString( MEMENTO_AUTOCONNECT );
      final boolean isAutoConnect = !StringUtils.isBlank( autoConnectName );
      m_autoConnectData.setAutoConnectName( autoConnectName );
      m_autoConnectData.setAutoConnect( isAutoConnect );
    }
  }

  @Override
  public void saveState( final IMemento memento )
  {
    super.saveState( memento );

    if( memento != null )
    {
      if( m_autoConnectData.getAutoConnect() )
        memento.putString( MEMENTO_AUTOCONNECT, m_autoConnectData.getAutoConnectName() );
    }
  }

  @Override
  public void createPartControl( final Composite parent )
  {
    m_toolkit = ToolkitUtils.createToolkit( parent );

    m_form = m_toolkit.createForm( parent );
    m_toolkit.decorateFormHeading( m_form );

    m_form.setImage( getFormImage() );
    m_form.setText( getFormTitel() );

    m_form.addMessageHyperlinkListener( m_statusListener );

    final IToolBarManager formToolbar = m_form.getToolBarManager();
    formToolbar.add( m_infoAction );
    formToolbar.add( m_disconnectAction );
    formToolbar.update( true );

    final Composite body = m_form.getBody();
    body.setLayout( new FillLayout() );

    m_updateControlJob.schedule( 250 );

    PlatformUI.getWorkbench().getHelpSystem().setHelp( m_form, IPdbHelp.CONTEXT_PDB_USER_MANUAL );
  }

  private Image getFormImage( )
  {
    if( m_pdbConnection == null )
      return WspmPdbUiImages.getImage( IMAGE.PDB_DISCONNECTED );

    return WspmPdbUiImages.getImage( IMAGE.PDB_CONNECTED );
  }

  private String getFormTitel( )
  {
    if( m_wspmProject == null )
      return Messages.getString( "PdbView.1" ); //$NON-NLS-1$

    if( m_pdbConnection == null )
      return Messages.getString( "PdbView.2" ); //$NON-NLS-1$

    final String label = m_pdbConnection.getLabel();
    return String.format( "%s", label ); //$NON-NLS-1$
  }

  private void startAutoConnect( )
  {
    if( m_autoConnectWasDone == true )
      return;

    m_autoConnectWasDone = true;

    /* If we do not auto-connect -> update control and show that we are not connected */
    if( !m_autoConnectData.getAutoConnect() )
      return;

    /* Start auto-connection */
    final String settingsName = m_autoConnectData.getAutoConnectName();
    final IStatus result = doConnect( settingsName );
    if( !result.isOK() )
      StatusDialog.open( getSite().getShell(), result, Messages.getString( "PdbView.4" ) ); //$NON-NLS-1$
  }

  private IStatus doConnect( final String settingsName )
  {
    try
    {
      final IPdbSettings settings = PdbSettings.getSettings( settingsName );
      final OpenConnectionThreadedOperation operation = new OpenConnectionThreadedOperation( settings, false );
      final IStatus result = ProgressUtilities.busyCursorWhile( operation );
      final IPdbConnection connection = operation.getConnection();
      setConnection( connection, result );

      return result;
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, Messages.getString( "PdbView.5" ), e ); //$NON-NLS-1$
    }
  }

  @Override
  public void setFocus( )
  {
    if( m_form != null && !m_form.isDisposed() )
      m_form.setFocus();
  }

  synchronized void setConnection( final IPdbConnection connection, final IStatus status )
  {
    Assert.isTrue( connection == null || connection.isConnected() );

    setStatus( status );

    /* If same instance -> nothing to do */
    if( connection == m_pdbConnection )
      return;

    try
    {
      /* Close old connection */
      if( m_pdbConnection != null )
        m_pdbConnection.close();
    }
    catch( final PdbConnectException e )
    {
      // FIXME: what to do now?
      e.printStackTrace();
    }

    m_pdbConnection = validateConnection( connection );
    final String settingsName = m_pdbConnection == null ? null : m_pdbConnection.getSettings().getName();
    m_autoConnectData.setAutoConnectName( settingsName );

    m_updateControlJob.schedule();
  }

  private IPdbConnection validateConnection( final IPdbConnection connection )
  {
    if( connection == null )
      return null;

    final Shell shell = getSite().getShell();
    final PdbUpdater checker = new PdbUpdater( connection, shell );
    final IStatus checkResult = checker.execute();
    setStatus( checkResult );

    if( checkResult.isOK() )
      return connection;

    /* Problem: set status and show dialog */
    if( !checkResult.matches( IStatus.CANCEL ) )
      StatusDialog.open( shell, checkResult, Messages.getString( "PdbView.3" ) ); //$NON-NLS-1$

    PdbUtils.closeQuietly( connection );
    return null;
  }

  private void setStatus( final IStatus status )
  {
    m_connectionStatus = status;
    MessageUtilitites.setMessage( m_form, status );
  }

  protected void updateControl( )
  {
    if( m_form.isDisposed() )
      return;

    m_connectionViewer = null;

    final Composite body = m_form.getBody();
    ControlUtils.disposeChildren( body );

    /* After each connect, we re-initialize the pdb project and its perspective */
    m_wspmProject = WspmPdbUiPlugin.getDefault().getWspmProject();

    m_form.setImage( getFormImage() );
    m_form.setText( getFormTitel() );

    final boolean isConnected = m_pdbConnection != null;

    m_disconnectAction.setEnabled( isConnected );
    m_infoAction.setEnabled( isConnected );

    final IWorkbenchPartSite site = getSite();

    if( m_wspmProject == null )
      createNoWspmProjectControl( m_toolkit, body );
    else if( isConnected )
      m_connectionViewer = new ConnectionViewer( PlatformUI.getWorkbench(), m_toolkit, body, m_pdbConnection, m_wspmProject );
    else
      new NonConnectedControl( m_toolkit, body, m_autoConnectData, this );

    if( m_connectionViewer != null )
      m_connectionViewer.createContextMenu( site );

    if( m_wspmProject != null && !isConnected )
      startAutoConnect();

    m_form.layout();

    /* activate map view */
    // REMARK: this actually fixes two bugs:
    // 1) the outline will show the map legend
    // 2) the context men uon the pdb works correctly (evaluation context was not correctly set)
    activateMapView();

    final IEvaluationService es = (IEvaluationService)PlatformUI.getWorkbench().getService( IEvaluationService.class );
    es.requestEvaluation( "pdbTester.hasRole" ); //$NON-NLS-1$

// final ICommandService cs = (ICommandService) PlatformUI.getWorkbench().getService( ICommandService.class );
// cs.refreshElements( "org.kalypso.model.wspm.pdb.ui.gmvtree.command.checkinState", null );
  }

  private void activateMapView( )
  {
    try
    {
      final IWorkbenchPage page = getSite().getWorkbenchWindow().getActivePage();
      if( page == null )
        return;

      page.showView( WspmMapViewPart.ID, null, IWorkbenchPage.VIEW_ACTIVATE );
    }
    catch( final PartInitException e )
    {
      e.printStackTrace();
    }
  }

  private void createNoWspmProjectControl( final FormToolkit toolkit, final Composite parent )
  {
    toolkit.createComposite( parent );
  }

  @Override
  public IPdbConnection getConnection( )
  {
    return m_pdbConnection;
  }

  public static void updateView( final IWorkbenchWindow window )
  {
    /* Do not restore, do not update if not created yet */
    final FindViewRunnable<PdbView> runnable = new FindViewRunnable<>( PdbView.ID, window, false );
    final PdbView view = runnable.execute();
    if( view == null )
      return;

    view.updateControl();
  }

  public static void reloadViewAndBringtoTop( final IWorkbenchWindow window, final String stateToSelect )
  {
    final ElementSelector elementSelector = new ElementSelector();
    elementSelector.addStateName( stateToSelect );

    reloadViewAndBringtoTop( window, elementSelector );
  }

  public static void reloadViewAndBringtoTop( final IWorkbenchWindow window, final ElementSelector selector )
  {
    /* Do not restore, do not update if not created yet */
    final FindViewRunnable<PdbView> runnable = new FindViewRunnable<>( PdbView.ID, window, false );
    final PdbView view = runnable.execute();
    if( view == null )
      return;

    final IWorkbenchPage page = window.getActivePage();
    page.activate( view );

    view.reload( selector );
  }

  @Override
  public void reload( final ElementSelector elementToSelect )
  {
    if( m_connectionViewer != null )
      m_connectionViewer.reload( elementToSelect );
  }

  @Override
  public String getUsername( )
  {
    return m_pdbConnection.getSettings().getUsername();
  }

  @Override
  public IWaterBodyStructure getStructure( )
  {
    if( m_connectionViewer == null )
      return null;

    return m_connectionViewer.getStructure();
  }

  @Override
  public PdbWspmProject getProject( )
  {
    if( m_connectionViewer == null )
      return null;

    return m_connectionViewer.getProject();
  }

  protected void openConnectionStatus( )
  {
    if( m_connectionStatus == null )
      return;

    new StatusDialog( getSite().getShell(), m_connectionStatus, Messages.getString( "PdbView.7" ) ).open(); //$NON-NLS-1$
  }

  public PdbInfo getInfo( )
  {
    if( m_pdbConnection == null )
      return null;

    return m_pdbConnection.getInfo();
  }

  @Override
  public IStructuredSelection getSelection( )
  {
    // thread safe
    final ConnectionViewer viewer = m_connectionViewer;
    if( viewer == null )
      return StructuredSelection.EMPTY;

    return viewer.getSelection();
  }
}