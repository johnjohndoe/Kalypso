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
package org.kalypso.model.wspm.tuhh.ui.actions;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.action.ProfileSelection;

/**
 * Common helper code for profile {@link org.eclipse.core.commands.IHandler}'s.
 * 
 * @author Gernot Belger
 */
public final class ProfileHandlerUtils
{
  private ProfileHandlerUtils( )
  {
    throw new UnsupportedOperationException( "Helper class, do not instantiate" ); //$NON-NLS-1$
  }

  public static ProfileSelection getProfileSelectionChecked( final ExecutionEvent event ) throws ExecutionException
  {
    final ProfileSelection profileSelection = getProfileSelection( event );
    if( profileSelection.hasProfiles() )
      return profileSelection;

    final String message = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.export.AbstractExportProfilesHandler.0" ); //$NON-NLS-1$
    throw new ExecutionException( message );
  }

  public static ProfileSelection getProfileSelectionOrShowMessage( final ExecutionEvent event ) throws ExecutionException
  {
    final Shell shell = HandlerUtil.getActiveShellChecked( event );
    final ProfileSelection profileSelection = getProfileSelection( event );

    if( profileSelection.hasProfiles() )
      return profileSelection;

    final String title = Messages.getString("ProfileHandlerUtils.0"); //$NON-NLS-1$
    final String message = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.export.AbstractExportProfilesHandler.0" ); //$NON-NLS-1$
    MessageDialog.openWarning( shell, title, message );
    return null;
  }

  public static ProfileSelection getProfileSelection( final ExecutionEvent event ) throws ExecutionException
  {
    final ISelection selection = HandlerUtil.getCurrentSelectionChecked( event );
    return new ProfileSelection( selection );
  }

}
