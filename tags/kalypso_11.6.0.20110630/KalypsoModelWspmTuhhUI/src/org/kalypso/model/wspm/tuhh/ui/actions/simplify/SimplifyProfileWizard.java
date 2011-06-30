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
package org.kalypso.model.wspm.tuhh.ui.actions.simplify;


import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.profil.wizard.ManipulateProfileWizard;
import org.kalypso.model.wspm.ui.profil.wizard.ProfileManipulationOperation.IProfileManipulator;

/**
 * @author Gernot Belger
 */
public class SimplifyProfileWizard extends ManipulateProfileWizard
{
  private SimplifyProfilePage m_simplifyPage;

  public SimplifyProfileWizard( )
  {
    setWindowTitle( Messages.getString( "SimplifyProfileHandler_0" ) ); //$NON-NLS-1$
  }

  @Override
  protected String getProfilePageMessage( )
  {
    return Messages.getString( "SimplifyProfileWizard_0" ); //$NON-NLS-1$
  }

  @Override
  public void addPages( )
  {
    m_simplifyPage = new SimplifyProfilePage( "simplifyPage" ); //$NON-NLS-1$

    addPage( m_simplifyPage );
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.wizard.ManipulateProfileWizard#getProfileManipulator()
   */
  @Override
  protected IProfileManipulator getProfileManipulator( )
  {
    final SimplifyProfilePage simplifyPage = m_simplifyPage;
    final double allowedDistance = simplifyPage.getDistance();

    final SimplifyProfileManipulator operation = new SimplifyProfileManipulator( allowedDistance, simplifyPage );
    operation.setKeepBuildingPoints( simplifyPage.isKeepBuildingPoints() );
    return operation;
  }
}
