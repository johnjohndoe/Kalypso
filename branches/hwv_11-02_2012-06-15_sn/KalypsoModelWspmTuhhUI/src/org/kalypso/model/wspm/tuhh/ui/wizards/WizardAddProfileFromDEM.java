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
package org.kalypso.model.wspm.tuhh.ui.wizards;

import org.eclipse.jface.wizard.Wizard;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author barbarins
 */
public class WizardAddProfileFromDEM extends Wizard
{
  private final ProfileFromDEMWizardPage m_page;

  private final IProfil m_profile;

  public WizardAddProfileFromDEM( final IProfil profile )
  {
    m_profile = profile;

    setWindowTitle( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizard.CreateProfileFromDem.3" ) ); //$NON-NLS-1$

    m_page = new ProfileFromDEMWizardPage( m_profile );
    addPage( m_page );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    m_profile.setName( m_page.getProfileName() );
    final double profileStation = m_page.getProfileStation();
    if( !Double.isNaN( profileStation ) )
      m_profile.setStation( profileStation );

    return true;
  }
}
