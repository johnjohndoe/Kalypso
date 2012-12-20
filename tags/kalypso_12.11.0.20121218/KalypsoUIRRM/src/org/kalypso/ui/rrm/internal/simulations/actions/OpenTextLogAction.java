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
package org.kalypso.ui.rrm.internal.simulations.actions;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FilenameUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.program.Program;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * This action opens a text file in a text editor.
 *
 * @author Holger Albert
 */
public class OpenTextLogAction extends Action implements IUpdateable
{
  private static final String DEFAULT_PROGRAM_EXTENSION = "txt"; //$NON-NLS-1$

  /**
   * The text file.
   */
  private final IFile m_textFile;

  private final String m_programExtension;

  public OpenTextLogAction( final String text, final String tooltipText, final IFile textFile )
  {
    this( text, tooltipText, textFile, DEFAULT_PROGRAM_EXTENSION );
  }

  /**
   * @param text
   *          The text.
   * @param tooltipText
   *          The tooltip text.
   * @param textFile
   *          The text file.
   * @param programExtension
   *          If non <code>null</code>, We will search for a program via this extension. Else the extension of the given
   *          file is used.
   */
  public OpenTextLogAction( final String text, final String tooltipText, final IFile textFile, final String programExtension )
  {
    super( text );

    m_programExtension = programExtension;

    setToolTipText( tooltipText );
    setImageDescriptor( UIRrmImages.id( UIRrmImages.DESCRIPTORS.OPEN_TEXT_LOG_ACTION ) );

    m_textFile = textFile;
  }

  @Override
  public void run( )
  {
    try
    {
      /* Get the file. */
      final File textFile = m_textFile.getLocation().toFile();

      /* Check if the text file exists. */
      if( !textFile.exists() )
        throw new IOException( String.format( Messages.getString( "OpenTextLogAction_0" ), textFile.getName() ) ); //$NON-NLS-1$

      /* Find the text editor registered for txt files. */
      final String programExtension = getProgrammExtension( textFile );
      final Program program = Program.findProgram( programExtension );
      if( program == null )
      {
        Program.launch( textFile.getAbsolutePath() );
        return;
      }

      /* Open the text editor with the text file. */
      program.execute( textFile.getAbsolutePath() );
    }
    catch( final Exception ex )
    {
      /* Display the error. */
      final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
      final String dialogTitle = getText();
      final String message = Messages.getString( "OpenTextLogAction_2" ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex );
      ErrorDialog.openError( shell, dialogTitle, message, status );
    }
  }

  private String getProgrammExtension( final File textFile )
  {
    if( m_programExtension != null )
      return m_programExtension;

    /* Fall back to own extension */
    final String name = textFile.getName();
    return FilenameUtils.getExtension( name );
  }

  @Override
  public void update( )
  {
    if( m_textFile.exists() )
      setEnabled( true );
    else
      setEnabled( false );
  }
}