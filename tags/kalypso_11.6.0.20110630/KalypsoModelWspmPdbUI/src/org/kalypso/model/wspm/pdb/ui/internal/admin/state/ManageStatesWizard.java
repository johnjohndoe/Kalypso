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
package org.kalypso.model.wspm.pdb.ui.internal.admin.state;

import org.eclipse.jface.wizard.Wizard;
import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;

/**
 * @author Gernot Belger
 */
public class ManageStatesWizard extends Wizard
{
  private final Session m_session;

  public ManageStatesWizard( final IPdbConnection connection )
  {
    m_session = openSession( connection );

    final String username = connection.getSettings().getUsername();

    addPage( new ManageStatesPage( "states", m_session, username ) ); //$NON-NLS-1$
  }

  private Session openSession( final IPdbConnection connection )
  {
    try
    {
      return connection.openSession();
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  @Override
  public void dispose( )
  {
    PdbUtils.closeSessionQuietly( m_session );

    super.dispose();
  }

  @Override
  public boolean performFinish( )
  {
    return true;
  }
}