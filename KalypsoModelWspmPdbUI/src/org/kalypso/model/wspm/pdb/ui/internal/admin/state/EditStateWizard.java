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

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.ui.internal.admin.gaf.UniqueStateNameValidator;
import org.kalypso.model.wspm.pdb.ui.internal.admin.state.EditStatePage.Mode;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class EditStateWizard extends Wizard implements IStateNamesProvider
{
  private final State m_state;

  private final Set<String> m_forbiddenStateNames;

  public EditStateWizard( final String[] existingStateNames, final State state )
  {
    m_state = state;
    m_forbiddenStateNames = new HashSet<>( Arrays.asList( existingStateNames ) );

    /* Ignore own name in edit mode */
    final String currentStateName = m_state.getName();
    m_forbiddenStateNames.remove( currentStateName );

    setWindowTitle( Messages.getString( "EditStateWizard.0" ) ); //$NON-NLS-1$
  }

  @Override
  public void addPages( )
  {
    final IValidator stateNameValidator = new UniqueStateNameValidator( this );

    final EditStatePage editStatePage = new EditStatePage( "editState", m_state, Mode.EDIT, stateNameValidator ); //$NON-NLS-1$
    editStatePage.setTitle( Messages.getString( "EditStateWizard.1" ) ); //$NON-NLS-1$
    editStatePage.setDescription( Messages.getString( "EditStateWizard.2" ) ); //$NON-NLS-1$
    addPage( editStatePage );
  }

  @Override
  public boolean performFinish( )
  {
    return true;
  }

  @Override
  public boolean isForbidden( final String stateName )
  {
    return m_forbiddenStateNames.contains( stateName );
  }
}