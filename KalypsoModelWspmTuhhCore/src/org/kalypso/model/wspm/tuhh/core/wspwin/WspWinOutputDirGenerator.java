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
package org.kalypso.model.wspm.tuhh.core.wspwin;

import java.io.File;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class WspWinOutputDirGenerator
{
  private enum AskMode
  {
    ALWAYS,
    NEVER,
    NOTSET;
  }

  private AskMode m_askMode = AskMode.NOTSET;

  private final File m_exportDir;

  private final boolean m_overwriteExisting;

  public WspWinOutputDirGenerator( final File exportDir, final boolean overwriteExisting )
  {
    m_exportDir = exportDir;
    m_overwriteExisting = overwriteExisting;
  }

  public File generateOutputDir( final String outputName, final String fallback ) throws CoreException
  {
    /* Fall back to id if name is blank */
    final String outputDirName = StringUtils.isBlank( outputName ) ? fallback : outputName;
    final String cleanOutputdirName = FileUtilities.validateName( outputDirName, "_" ); //$NON-NLS-1$
    final File dir = new File( m_exportDir, cleanOutputdirName );
    return checkExistance( dir );
  }

  private File checkExistance( final File dir ) throws CoreException
  {
    /* No check, if we should just overwrite */
    if( m_overwriteExisting )
      return dir;

    if( !dir.exists() )
      return dir;

    if( dir.isFile() )
    {
      final String msg = String.format( Messages.getString("WspWinOutputDirGenerator_0"), dir.getName() ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhCorePlugin.PLUGIN_ID, msg );
      throw new CoreException( status );
    }

    switch( m_askMode )
    {
      case ALWAYS:
        return dir;
      case NEVER:
        return null;

      case NOTSET:
      default:
        return askForExistence( dir );
    }
  }

  private File askForExistence( final File dir )
  {
    final String message = String.format( Messages.getString("WspWinOutputDirGenerator_1"), dir.getName() ); //$NON-NLS-1$
    final String[] labels = new String[] { IDialogConstants.YES_LABEL, IDialogConstants.NO_LABEL, Messages.getString("WspWinOutputDirGenerator_2"), Messages.getString("WspWinOutputDirGenerator_3"), IDialogConstants.CANCEL_LABEL }; //$NON-NLS-1$ //$NON-NLS-2$

    final Shell shell = SWT_AWT_Utilities.findActiveShell();
    final MessageDialog dialog = new MessageDialog( shell, Messages.getString("WspWinOutputDirGenerator_4"), null, message, MessageDialog.QUESTION, labels, 0 ); //$NON-NLS-1$
    switch( SWT_AWT_Utilities.openSwtWindow( dialog ) )
    {
      case 0:
        return dir;

      case 1:
        return null;

      case 2:
        m_askMode = AskMode.ALWAYS;
        return dir;

      case 3:
        m_askMode = AskMode.NEVER;
        return null;

      case 4:
        throw new OperationCanceledException();
      default:
        throw new IllegalStateException();
    }
  }
}