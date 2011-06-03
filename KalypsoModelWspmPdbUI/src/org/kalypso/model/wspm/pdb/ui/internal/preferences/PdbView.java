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
package org.kalypso.model.wspm.pdb.ui.internal.preferences;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.contribs.eclipse.ui.forms.ToolkitUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiImages;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiImages.IMAGE;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.FindViewRunnable;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.PdbWspmProject;

/**
 * @author Gernot Belger
 */
public class PdbView extends ViewPart
{
  public static final String ID = "PdbView"; //$NON-NLS-1$

  private static final String MEMENTO_AUTOCONNECT = "autoconnect.name"; //$NON-NLS-1$

  private final Action m_disconnectAction = new DisconnectPdbAction( this );

  private final UIJob m_updateControlJob = new UIJob( "Update pdb view" )
  {
    @Override
    public IStatus runInUIThread( final IProgressMonitor monitor )
    {
      updateControl();
      return Status.OK_STATUS;
    }
  };

  private Form m_form;

  private FormToolkit m_toolkit;

  private IPdbConnection m_pdbConnection;

  private final OpenConnectionData m_autoConnectData = new OpenConnectionData();

  private PdbWspmProject m_wspmProject;

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

    final IToolBarManager formToolbar = m_form.getToolBarManager();
    formToolbar.add( m_disconnectAction );
    formToolbar.update( true );

    final Composite body = m_form.getBody();
    body.setLayout( new FillLayout() );

    updateControl();

    startAutoConnect();
  }

  private Image getFormImage( )
  {
    if( m_pdbConnection == null )
      return WspmPdbUiImages.getImage( IMAGE.PDB_DISCONNECTED );

    return WspmPdbUiImages.getImage( IMAGE.PDB_CONNECTED );
  }

  private String getFormTitel( )
  {
    if( m_pdbConnection == null )
      return "<Not Connected>";

    final String label = m_pdbConnection.getLabel();
    return String.format( "%s", label );
  }

  private void startAutoConnect( )
  {
    /* If we do not auto-connect -> update control and show that we are not connected */
    if( !m_autoConnectData.getAutoConnect() )
      return;

    /* Start auto-connection */
    final PdbConnectJob autoConnector = new PdbConnectJob( m_autoConnectData.getAutoConnectName() );
    autoConnector.setUser( true );

    autoConnector.addJobChangeListener( new JobChangeAdapter()
    {
      @Override
      public void done( final IJobChangeEvent event )
      {
        final IPdbConnection connection = autoConnector.getConnection();
        setConnection( connection );
      }
    } );

    autoConnector.schedule();
  }

  @Override
  public void setFocus( )
  {
    if( m_form != null && !m_form.isDisposed() )
      m_form.setFocus();
  }

  synchronized void setConnection( final IPdbConnection connection )
  {
    Assert.isTrue( connection == null || connection.isConnected() );

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

    m_pdbConnection = connection;
    final String settingsName = m_pdbConnection == null ? null : m_pdbConnection.getSettings().getName();
    m_autoConnectData.setAutoConnectName( settingsName );

    m_updateControlJob.schedule();
  }

  protected void updateControl( )
  {
    final Composite body = m_form.getBody();
    ControlUtils.disposeChildren( body );

    m_form.setImage( getFormImage() );
    m_form.setText( getFormTitel() );

    /* After each connect, we re-initialize the pdb project and its perspective */
    m_wspmProject = WspmPdbUiPlugin.getDefault().getWspmProject();

    final boolean isConnected = m_pdbConnection != null;

    m_disconnectAction.setEnabled( isConnected );

    if( m_wspmProject == null )
      createNoWspmProjectControl( m_toolkit, body );
    else if( isConnected )
      new ConnectionViewer( m_toolkit, body, m_pdbConnection, m_wspmProject );
    else
      new NonConnectedControl( m_toolkit, body, m_autoConnectData, this );

    m_form.layout();
  }

  private void createNoWspmProjectControl( final FormToolkit toolkit, final Composite parent )
  {
    toolkit.createLabel( parent, "WSPM project not initialized" );
  }

  IPdbConnection getConnection( )
  {
    return m_pdbConnection;
  }

  public static void updateView( final IWorkbenchWindow window )
  {
    final FindViewRunnable<PdbView> runnable = new FindViewRunnable<PdbView>( PdbView.ID, window );
    final PdbView view = runnable.execute();
    if( view == null )
      return;

    view.updateControl();
  }
}