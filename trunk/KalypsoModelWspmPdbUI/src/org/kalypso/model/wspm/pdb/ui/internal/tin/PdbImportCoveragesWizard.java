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
package org.kalypso.model.wspm.pdb.ui.internal.tin;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IPageChangingListener;
import org.eclipse.jface.dialogs.PageChangingEvent;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.IMessage;
import org.kalypso.contribs.eclipse.ui.forms.MessageUtilitites;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.ui.internal.PdbUiUtils;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.checkout.ConnectionChooserPage;
import org.kalypso.model.wspm.pdb.ui.internal.content.IConnectionViewer;
import org.kalypso.model.wspm.pdb.ui.internal.tin.data.PdbImportConnectionChooserData;

/**
 * This wizard shows a page to select from an entry (path) of the external storage location and imports the coverages.
 * 
 * @author Holger Albert
 */
public class PdbImportCoveragesWizard extends Wizard
{
  /**
   * This listener is notified, if the page has changed.
   */
  private final IPageChangingListener m_pageListener = new IPageChangingListener()
  {
    @Override
    public void handlePageChanging( final PageChangingEvent event )
    {
      pageChanging( event );
    }
  };

  /**
   * The data containing the connection settings.
   */
  private final PdbImportConnectionChooserData m_settingsData;

  /**
   * The constructor.
   */
  public PdbImportCoveragesWizard( )
  {
    m_settingsData = new PdbImportConnectionChooserData();

    setWindowTitle( "Höhendaten aus externen Speicherort hinzufügen" );
    setNeedsProgressMonitor( true );
  }

  @Override
  public boolean canFinish( )
  {
    final boolean canFinish = super.canFinish();
    final IWizardPage currentPage = getContainer().getCurrentPage();
    return canFinish && !(currentPage instanceof ConnectionChooserPage);
  }

  @Override
  public void setContainer( final IWizardContainer container )
  {
    final IWizardContainer oldContainer = getContainer();
    if( oldContainer instanceof WizardDialog )
      ((WizardDialog) oldContainer).removePageChangingListener( m_pageListener );

    super.setContainer( container );

    if( container instanceof WizardDialog )
      ((WizardDialog) container).addPageChangingListener( m_pageListener );
  }

  @Override
  public void addPages( )
  {
    final IStatus connectionStatus = checkConnection();
    if( connectionStatus.getSeverity() == IStatus.ERROR )
    {
      // TODO
    }

    if( connectionStatus.getSeverity() == IStatus.WARNING )
      addPage( new ConnectionChooserPage( "connectionChooser", m_settingsData ) ); //$NON-NLS-1$

    addPage( new SearchDhmIndexPage( "searchDhmIndex", m_settingsData ) ); //$NON-NLS-1$
  }

  @Override
  public boolean performFinish( )
  {
    // TODO

    return false;
  }

  private IStatus checkConnection( )
  {
    /* Get the active workbench window. */
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();

    /* If there is a viewer, we are within the PDB perspective. */
    final IConnectionViewer connectionViewer = PdbUiUtils.getConnectionViewer( window );
    if( connectionViewer != null )
    {
      /* We are within the PDB perspective. */
      final IPdbConnection connection = PdbUiUtils.getConnection( window );
      if( connection == null )
        return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, "Es besteht keine Verbindung zur Datenbank." );

      m_settingsData.setConnection( connection );

      return Status.OK_STATUS;
    }

    /* We are outside the PDB. */
    final IPdbConnection connection = PdbUiUtils.getConnection( window );
    if( connection == null )
      return new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, "Es besteht keine Verbindung zur Datenbank." );

    m_settingsData.setConnection( connection );

    return Status.OK_STATUS;
  }

  protected void pageChanging( final PageChangingEvent event )
  {
    final Object currentPage = event.getCurrentPage();
    if( currentPage instanceof ConnectionChooserPage )
      doConnect( event );
  }

  /**
   * This function connects to the database.
   */
  private void doConnect( final PageChangingEvent event )
  {
    /* Connect to the database. */
    final IStatus result = m_settingsData.doConnect( getContainer() );
    if( result.isOK() )
    {
      /* Change the page. */
      event.doit = true;

      return;
    }

    /* Do not change the page. */
    event.doit = false;

    /* Set a message to the current page. */
    final IMessage message = MessageUtilitites.convertStatus( result );
    ((WizardPage) event.getCurrentPage()).setMessage( message.getMessage(), message.getMessageType() );
  }
}