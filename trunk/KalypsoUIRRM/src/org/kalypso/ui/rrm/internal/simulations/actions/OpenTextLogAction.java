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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.program.Program;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.UIRrmImages;

/**
 * This action opens a text file in a text editor.
 * 
 * @author Holger Albert
 */
public class OpenTextLogAction extends Action
{
  /**
   * The simulation.
   */
  private final RrmSimulation m_simulation;

  /**
   * If true, the calculation.log will be opened. If false, the statistics.csv will be opened.
   */
  private final boolean m_calculationLog;

  /**
   * The constructor.
   * 
   * @param text
   *          The text.
   * @param tooltipText
   *          The tooltip text.
   * @param simulation
   *          The simulation.
   * @param calculationLog
   *          If true, the calculation.log will be opened. If false, the statistics.csv will be opened.
   */
  public OpenTextLogAction( final String text, final String tooltipText, final RrmSimulation simulation, final boolean calculationLog )
  {
    super( text );

    setToolTipText( tooltipText );

    m_simulation = simulation;
    m_calculationLog = calculationLog;
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  @Override
  public void run( )
  {
    try
    {
      /* Get the file. */
      File textFile = null;
      if( m_calculationLog )
      {
        final IFile calculationLog = m_simulation.getCalculationLog();
        textFile = calculationLog.getLocation().toFile();
      }
      else
      {
        final IFile statisticsCsv = m_simulation.getStatisticsCsv();
        textFile = statisticsCsv.getLocation().toFile();
      }

      /* Check if the text file exists. */
      if( !textFile.exists() )
        throw new IOException( String.format( "The text file '%s' does not exist...", textFile.getName() ) );

      /* Find the text editor registered for txt files. */
      final Program program = Program.findProgram( "txt" );
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