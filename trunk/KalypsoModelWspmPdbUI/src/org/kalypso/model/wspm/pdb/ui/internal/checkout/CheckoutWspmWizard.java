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
package org.kalypso.model.wspm.pdb.ui.internal.checkout;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IPageChangingListener;
import org.eclipse.jface.dialogs.PageChangingEvent;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.forms.IMessage;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.ui.forms.MessageUtilitites;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;
import org.kalypso.model.wspm.pdb.db.OpenConnectionThreadedOperation;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.ConnectionChooserData;
import org.kalypso.model.wspm.pdb.wspm.CheckoutPdbData;
import org.kalypso.model.wspm.pdb.wspm.CheckoutPdbOperation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.ui.imports.WspmTuhhProjectSelection;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * @author Gernot Belger
 */
public class CheckoutWspmWizard extends Wizard implements IWorkbenchWizard
{
  private final IPageChangingListener m_pageListener = new IPageChangingListener()
  {
    @Override
    public void handlePageChanging( final PageChangingEvent event )
    {
      pageChanging( event );
    }
  };

  private final ConnectionChooserData m_settingsData = new ConnectionChooserData();

  private final CheckoutPdbData m_checkoutData = new CheckoutPdbData();

  private CommandableWorkspace m_workspace;

  private TuhhWspmProject m_project;

  private PdbContentPage m_contentPage;

  public CheckoutWspmWizard( )
  {
    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "CheckoutWspmWizard.0" ) ); //$NON-NLS-1$
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    final WspmTuhhProjectSelection projectSelection = new WspmTuhhProjectSelection( selection );
    m_workspace = projectSelection.getWorkspace();
    m_project = projectSelection.getProject();

    Assert.isTrue( m_workspace != null );
    Assert.isTrue( m_project != null );
  }

  @Override
  public void createPageControls( final Composite pageContainer )
  {
    // do not allow to create pages early
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

  /**
   * Do not finish early.
   * 
   * @see org.eclipse.jface.wizard.Wizard#canFinish()
   */
  @Override
  public boolean canFinish( )
  {
    final boolean canFinish = super.canFinish();
    final IWizardPage currentPage = getContainer().getCurrentPage();
    return canFinish && currentPage instanceof CheckoutPdbPreviewPage;
  }

  @Override
  public void addPages( )
  {
    addPage( new ConnectionChooserPage( "connectionChooser", m_settingsData ) ); //$NON-NLS-1$

    m_contentPage = new PdbContentPage( "elementChooser" ); //$NON-NLS-1$
    addPage( m_contentPage );

    addPage( new CheckoutPdbPreviewPage( "previewPage", m_checkoutData ) ); //$NON-NLS-1$
  }

  @Override
  public boolean performFinish( )
  {
    final CheckoutPdbOperation operation = new CheckoutPdbOperation( m_checkoutData );
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, operation );
    if( !status.isOK() )
      new StatusDialog( getShell(), status, getWindowTitle() ).open();

    return !status.matches( IStatus.ERROR );
  }

  @Override
  public void dispose( )
  {
    m_checkoutData.closeConnection();
  }

  protected void pageChanging( final PageChangingEvent event )
  {
    final Object currentPage = event.getCurrentPage();
    if( currentPage instanceof ConnectionChooserPage )
      doConnect( event );
    else if( currentPage == m_contentPage )
      doSelectContent( event );
  }

  private void doConnect( final PageChangingEvent event )
  {
    final IPdbSettings settings = m_settingsData.getSettings();

    final IStatus result = doConnectAndInitData( settings );
    if( !result.isOK() )
    {
      event.doit = false;
      final IMessage message = MessageUtilitites.convertStatus( result );
      ((WizardPage) event.getCurrentPage()).setMessage( message.getMessage(), message.getMessageType() );
    }
  }

  private IStatus doConnectAndInitData( final IPdbSettings settings )
  {
    final OpenConnectionThreadedOperation operation = new OpenConnectionThreadedOperation( settings, false );
    final IStatus result = RunnableContextHelper.execute( getContainer(), true, true, operation );

    if( !result.isOK() )
      return result;

    final IPdbConnection connection = operation.getConnection();
    final IStatus initStatus = m_checkoutData.init( getShell(), getWindowTitle(), getDialogSettings(), connection );
    if( !initStatus.isOK() )
      return initStatus;

    m_contentPage.setConnection( connection );

    return Status.OK_STATUS;
  }

  private void doSelectContent( final PageChangingEvent event )
  {
    final IStructuredSelection selection = m_contentPage.getSelection();
    if( selection.isEmpty() )
    {
      event.doit = false;
      return;
    }

    m_checkoutData.initMapping( selection, m_workspace, m_project );
  }
}