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
package org.kalypso.model.wspm.tuhh.ui.export;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.model.wspm.tuhh.ui.actions.ProfileHandlerUtils;
import org.kalypso.model.wspm.ui.action.ProfileSelection;

/**
 * Abstract handler that should be used for exporting profiles.
 * 
 * @author Gernot Belger
 */
public abstract class AbstractExportProfilesHandler extends AbstractHandler
{
  /**
   * @see org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public final Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final Shell shell = HandlerUtil.getActiveShellChecked( event );
    final ProfileSelection profileSelection = ProfileHandlerUtils.getProfileSelectionOrShowMessage( event );

    if( !checkPrerequisites( shell, event ) )
      return null;

    final IWizard exportProfileWizard = createWizard( event, profileSelection );

    /* show wizard */
    final WizardDialog2 dialog = new WizardDialog2( shell, exportProfileWizard );
    dialog.setRememberSize( true );
    dialog.open();

    return null;
  }

  /**
   * Allow the implementor to check any prerequisites before the wizard is created and opened. <br/>
   * Intended to be overwritten by implementors.
   * 
   * @return <code>true</code>
   */
  @SuppressWarnings("unused")
  protected boolean checkPrerequisites( final Shell shell, final ExecutionEvent event )
  {
    return true;
  }

  protected abstract IWizard createWizard( ExecutionEvent event, final ProfileSelection selection ) throws ExecutionException;
}
