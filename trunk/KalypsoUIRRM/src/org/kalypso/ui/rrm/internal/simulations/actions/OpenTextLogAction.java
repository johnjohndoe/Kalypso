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
package org.kalypso.ui.rrm.internal.simulations.actions;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.program.Program;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.UIRrmImages;

/**
 * This action opens a text file in the linked system editor.
 * 
 * @author Holger Albert
 */
public class OpenTextLogAction extends Action
{
  /**
   * The text file.
   */
  private final File m_textFile;

  /**
   * The constructor.
   * 
   * @param text
   *          The text.
   * @param tooltipText
   *          The tooltip text.
   * @param textFile
   *          The text file.
   */
  public OpenTextLogAction( final String text, final String tooltipText, final File textFile )
  {
    super( text );

    setToolTipText( tooltipText );

    m_textFile = textFile;
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  @Override
  public void run( )
  {
    try
    {
      /* Check, if a text file was provided. */
      if( m_textFile == null )
        throw new IllegalArgumentException( "No text file given..." );

      /* Check, if the text file exists. */
      if( !m_textFile.exists() )
        throw new IOException( String.format( "The text file '%s' does not exist...", m_textFile.getAbsolutePath() ) );

      /* Launch the system dependend editor. */
      Program.launch( m_textFile.getAbsolutePath() );
    }
    catch( final Exception ex )
    {
      final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
      final String dialogTitle = getText();
      final String message = "The file could not be opened...";
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex );
      ErrorDialog.openError( shell, dialogTitle, message, status );
    }
  }

  /**
   * @see org.eclipse.jface.action.Action#getImageDescriptor()
   */
  @Override
  public ImageDescriptor getImageDescriptor( )
  {
    return UIRrmImages.id( UIRrmImages.DESCRIPTORS.OPEN_TEXT_LOG_ACTION );
  }
}