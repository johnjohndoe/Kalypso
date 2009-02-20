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
package org.kalypso.util.swt;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

/**
 * @author Dirk Kuch
 */
public class AbstractStatusDialog extends MessageDialog
{
  private final IStatus m_status;

  public AbstractStatusDialog( final Shell parentShell, final IStatus status, final String dialogTitle )
  {
    super( parentShell, dialogTitle, null, StringUtils.abbreviate( status.getMessage(), 512 ), toMessageType( status.getSeverity() ), new String[] { IDialogConstants.OK_LABEL }, 0 );

    m_status = status;
  }

  protected IStatus getStatus( )
  {
    return m_status;
  }

  private static int toMessageType( final int severity )
  {
    switch( severity )
    {
      case IStatus.OK:
        return MessageDialog.NONE;

      case IStatus.INFO:
        return MessageDialog.INFORMATION;

      case IStatus.WARNING:
        return MessageDialog.WARNING;

      case IStatus.ERROR:
        return MessageDialog.ERROR;

      case IStatus.CANCEL:
        return MessageDialog.WARNING;

      default:
        return MessageDialog.NONE;
    }
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected void createButtonsForButtonBar( final Composite parent )
  {
    createButton( parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true );
  }
}