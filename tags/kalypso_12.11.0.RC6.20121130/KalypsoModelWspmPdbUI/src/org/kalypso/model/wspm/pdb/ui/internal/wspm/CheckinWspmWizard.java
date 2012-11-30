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
package org.kalypso.model.wspm.pdb.ui.internal.wspm;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IMessageProvider;
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
import org.kalypso.contribs.eclipse.ui.forms.MessageUtilitites;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.core.gml.WspmFixation;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PDBRole;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.checkout.ConnectionChooserPage;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.gml.CalculationWspmTuhhSteadyState;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class CheckinWspmWizard extends Wizard implements IWorkbenchWizard
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

  private ICheckInWorker m_worker;

  public CheckinWspmWizard( )
  {
    setWindowTitle( Messages.getString( "CheckinWspmWizard.2" ) ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );
  }

  @Override
  public void createPageControls( final Composite pageContainer )
  {
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
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    Assert.isTrue( selection instanceof IFeatureSelection );
    final Object firstElement = selection.getFirstElement();
    Assert.isTrue( firstElement instanceof Feature );

    final CommandableWorkspace workspace = ((IFeatureSelection) selection).getWorkspace( (Feature) firstElement );

    m_worker = createWorker( workspace, firstElement );
    Assert.isNotNull( m_worker );
  }

  private ICheckInWorker createWorker( final CommandableWorkspace workspace, final Object element )
  {
    if( element instanceof TuhhReach )
      return new CheckinStateWorker( workspace, (TuhhReach) element );

    if( element instanceof WspmFixation )
      return new CheckInEventWorker( workspace, (WspmFixation) element );

    if( element instanceof CalculationWspmTuhhSteadyState )
      return new CheckinCalculationWorker( workspace, (CalculationWspmTuhhSteadyState) element );

    return null;
  }

  @Override
  public void addPages( )
  {
    addPage( new ConnectionChooserPage( "connectionChooser", m_settingsData ) ); //$NON-NLS-1$

    /* Steal pages from worker wizard */
    final Wizard wizard = m_worker.createWizard();
    wizard.addPages();
    final IWizardPage[] pages = wizard.getPages();
    for( final IWizardPage page : pages )
      addPage( page );

    wizard.dispose();
  }

  @Override
  public boolean performFinish( )
  {
    return m_worker.performFinish( getContainer() );
  }

  @Override
  public void dispose( )
  {
    m_worker.closeConnection();
  }

  protected void pageChanging( final PageChangingEvent event )
  {
    final Object currentPage = event.getCurrentPage();
    if( currentPage instanceof ConnectionChooserPage )
      doConnect( event );
  }

  private void doConnect( final PageChangingEvent event )
  {
    final IStatus result = m_settingsData.doConnect( getContainer() );
    if( result.isOK() )
    {
      final IPdbConnection connection = m_settingsData.getConnection();

      final PDBRole role = connection.getRole();
      if( role == PDBRole.user )
      {
        event.doit = false;
        ((WizardPage) event.getCurrentPage()).setMessage( Messages.getString( "CheckinWspmWizard.0" ), IMessageProvider.WARNING ); //$NON-NLS-1$
        return;
      }

      event.doit = initializeWorker( connection );
    }
    else
    {
      event.doit = false;
      final IMessage message = MessageUtilitites.convertStatus( result );
      ((WizardPage) event.getCurrentPage()).setMessage( message.getMessage(), message.getMessageType() );
    }
  }

  private boolean initializeWorker( final IPdbConnection connection )
  {
    final IStatus status = doPreinit( connection );
    if( !status.isOK() )
    {
      StatusDialog.open( getShell(), status, getWindowTitle() );
      return false;
    }

    return true;
  }

  private IStatus doPreinit( final IPdbConnection connection )
  {
    try
    {
      m_worker.preInit( connection );
      return m_worker.checkPreconditions();
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, Messages.getString( "CheckinWspmWizard.1" ) ); //$NON-NLS-1$
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      return e.getStatus();
    }

  }
}