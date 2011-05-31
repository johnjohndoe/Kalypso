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
package org.kalypso.model.wspm.pdb.ui.internal.admin.gaf;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.IPageChangeProvider;
import org.eclipse.jface.dialogs.IPageChangedListener;
import org.eclipse.jface.dialogs.PageChangedEvent;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.gaf.ImportGafData;
import org.kalypso.model.wspm.pdb.gaf.ImportGafOperation;
import org.kalypso.model.wspm.pdb.gaf.ReadGafOperation;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.admin.state.EditStatePage;
import org.kalypso.model.wspm.pdb.ui.internal.admin.state.EditStatePage.Mode;

public class ImportGafWizard extends Wizard
{
  private final IPageChangedListener m_pageListener = new IPageChangedListener()
  {
    @Override
    public void pageChanged( final PageChangedEvent event )
    {
      handlePageChanged( event.getSelectedPage() );
    }
  };

  private final ImportGafData m_data;

  private final GafProfilesPage m_gafProfilesPage;

  public ImportGafWizard( final State[] existingState, final IPdbConnection connection )
  {
    m_data = new ImportGafData( connection );

    final IDialogSettings settings = DialogSettingsUtils.getDialogSettings( WspmPdbUiPlugin.getDefault(), getClass().getName() );
    setDialogSettings( settings );

    m_data.init( settings );

    addPage( new ImportGafPage( "gaf", m_data ) ); //$NON-NLS-1$
    m_gafProfilesPage = new GafProfilesPage( "profiles", m_data ); //$NON-NLS-1$
    addPage( m_gafProfilesPage );
    // TODO: some options for gaf import: need to refaktor how parts are built
    // addPage( new GafOptionsPage( "options", m_data ) ); //$NON-NLS-1$
    addPage( new ChooseWaterPage( "waterBody", m_data ) ); //$NON-NLS-1$
    addPage( new EditStatePage( "state", m_data.getState(), existingState, Mode.NEW ) ); //$NON-NLS-1$

    setNeedsProgressMonitor( true );
  }

  @Override
  public void setContainer( final IWizardContainer container )
  {
    final IWizardContainer oldContainer = getContainer();
    if( oldContainer instanceof IPageChangeProvider )
      ((IPageChangeProvider) oldContainer).removePageChangedListener( m_pageListener );

    super.setContainer( container );

    if( container instanceof IPageChangeProvider )
      ((IPageChangeProvider) container).addPageChangedListener( m_pageListener );
  }

  @Override
  public boolean canFinish( )
  {
    /* Do not allow to finish early */
    final IWizardPage currentPage = getContainer().getCurrentPage();
    if( currentPage.getNextPage() != null )
      return false;

    return super.canFinish();
  }

  @Override
  public boolean performFinish( )
  {
    final ImportGafOperation operation = new ImportGafOperation( m_data );
    final IStatus result = RunnableContextHelper.execute( getContainer(), true, true, operation );
    new StatusDialog2( getShell(), result, getWindowTitle() );

    final IDialogSettings settings = getDialogSettings();
    if( settings != null )
      m_data.store( settings );

    return true;
  }

  protected void handlePageChanged( final Object selectedPage )
  {
    if( selectedPage == m_gafProfilesPage )
    {
      /* Prepare for exception */
      m_data.setProfiles( null );

      final ReadGafOperation operation = new ReadGafOperation( m_data );
      final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, operation );
      if( !status.isOK() )
        new StatusDialog2( getShell(), status, getWindowTitle() );
      else
        m_data.setProfiles( operation.getProfiles() );

      m_gafProfilesPage.updateControl();
    }
  }
}