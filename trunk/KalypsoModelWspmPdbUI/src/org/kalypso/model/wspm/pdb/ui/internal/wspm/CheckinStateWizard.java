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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.ui.internal.admin.state.EditStatePage;
import org.kalypso.model.wspm.pdb.ui.internal.admin.state.EditStatePage.Mode;
import org.kalypso.model.wspm.pdb.ui.internal.admin.state.IStatesProvider;
import org.kalypso.model.wspm.pdb.wspm.CheckinStateData;
import org.kalypso.model.wspm.pdb.wspm.CheckinStateWorker;

/**
 * Uploads local WSPM data into the cross section database.
 * 
 * @author Gernot Belger
 */
public class CheckinStateWizard extends Wizard implements IStatesProvider
{
  private final CheckinStateData m_data;

  private final IPdbConnection m_connection;

  private IStatus m_status;

  public CheckinStateWizard( final CheckinStateData data, final IPdbConnection connection )
  {
    m_data = data;
    m_connection = connection;

    setNeedsProgressMonitor( true );

    addPage( new CheckinStateChooseElementsPage( "chooseElements", m_data ) ); //$NON-NLS-1$
    addPage( new EditStatePage( "editState", m_data.getState(), this, Mode.NEW ) ); //$NON-NLS-1$
  }

  @Override
  public boolean performFinish( )
  {
    m_data.commitCheckedElements();

    final CheckinStateWorker operation = new CheckinStateWorker( m_data, m_connection );
    m_status = RunnableContextHelper.execute( getContainer(), true, true, operation );
    if( !m_status.isOK() )
      new StatusDialog2( getShell(), m_status, getWindowTitle() ).open();

    return m_status.isOK();
  }

  IStatus getStatus( )
  {
    return m_status;
  }

  @Override
  public State[] getStates( )
  {
    return m_data.getExistingStates();
  }
}