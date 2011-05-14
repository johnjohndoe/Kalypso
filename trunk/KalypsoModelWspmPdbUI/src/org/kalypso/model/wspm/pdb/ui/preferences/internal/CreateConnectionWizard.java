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
package org.kalypso.model.wspm.pdb.ui.preferences.internal;

import org.eclipse.jface.dialogs.IPageChangingListener;
import org.eclipse.jface.dialogs.PageChangingEvent;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.model.wspm.pdb.connect.IPdbConnectInfo;

/**
 * @author Gernot Belger
 */
class CreateConnectionWizard extends Wizard
{
  private final IPageChangingListener m_pageListener = new IPageChangingListener()
  {
    @Override
    public void handlePageChanging( final PageChangingEvent event )
    {
      CreateConnectionWizard.this.handlePageChanging();
    }
  };

  private final WspmPdbPreferencePage m_page;

  private IPdbConnectInfo m_info = null;

  private final ConnectionTypePage m_typePage;

  private final PdbConnectionPage m_connectionPage;

  public CreateConnectionWizard( final WspmPdbPreferencePage page )
  {
    m_page = page;

    m_typePage = new ConnectionTypePage( "type" ); //$NON-NLS-1$
    m_connectionPage = new PdbConnectionPage( "connection", null ); //$NON-NLS-1$

    setWindowTitle( "Create New Connection Settings" );
    setNeedsProgressMonitor( true );

    addPage( m_typePage );
    addPage( m_connectionPage );
  }

  @Override
  public boolean canFinish( )
  {
    if( getContainer().getCurrentPage() != m_connectionPage )
      return false;

    return super.canFinish();
  }

  @Override
  public boolean performFinish( )
  {
    m_page.addNewItem( m_info );
    return true;
  }

  public IPageChangingListener getPageListener( )
  {
    return m_pageListener;
  }

  protected void handlePageChanging( )
  {
    m_info = m_typePage.getInfo();
    m_connectionPage.setConnection( m_info );
    getContainer().updateButtons();
  }

  @Override
  public IWizardPage getPreviousPage( final IWizardPage page )
  {
    return null;
  }
}