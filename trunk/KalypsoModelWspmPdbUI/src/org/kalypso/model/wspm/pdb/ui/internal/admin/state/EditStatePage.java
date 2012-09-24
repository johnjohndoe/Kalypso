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

import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class EditStatePage extends WizardPage
{
  public static final String STR_ENTER_THE_PROPERTIES_OF_THE_FRESHLY_CREATED_STATE = Messages.getString( "EditStatePage.0" ); //$NON-NLS-1$

  public static final String STR_ENTER_STATE_PROPERTIES = Messages.getString( "EditStatePage.1" ); //$NON-NLS-1$

  public enum Mode
  {
    NEW,
    EDIT,
    VIEW;
  }

  private final Mode m_mode;

  private final State m_state;

  private DatabindingWizardPage m_binding;

  private final IValidator m_stateNameValidator;

  public EditStatePage( final String pageName, final State state, final Mode mode, final IValidator stateNameValidator )
  {
    super( pageName );

    m_state = state;
    m_mode = mode;
    m_stateNameValidator = stateNameValidator;
  }

  @Override
  public void createControl( final Composite parent )
  {
    m_binding = new DatabindingWizardPage( this, null );

    final StateViewer panel = new StateViewer( parent, m_binding, m_state, m_mode, m_stateNameValidator );
    setControl( panel );
  }
}