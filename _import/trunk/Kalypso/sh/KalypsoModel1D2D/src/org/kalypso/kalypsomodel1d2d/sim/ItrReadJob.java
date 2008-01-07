/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.kalypsomodel1d2d.sim;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.math.BigDecimal;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;

/**
 * @author Gernot Belger
 */
public class ItrReadJob extends Job
{
  private final File m_file;

  private int m_stepNr = -1;

  public ItrReadJob( final String name, final File file )
  {
    super( name );
    m_file = file;
  }

  public int getStepNr( )
  {
    return m_stepNr;
  }

  /**
   * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    monitor.beginTask( getName(), IProgressMonitor.UNKNOWN );

    LineNumberReader lnr = null;
    try
    {
      /* Wait until file exists */
      while( true )
      {
        if( m_file.exists() )
          break;

        try
        {
          Thread.sleep( 100 );
        }
        catch( final InterruptedException e )
        {
          e.printStackTrace();
        }

        ProgressUtilities.worked( monitor, 1 );
      }

      System.out.println( "file exists: " + m_file );

      // TODO: does not work!

      /* Read file and write outputs */
      lnr = new LineNumberReader( new FileReader( m_file ) );
      while( true )
      {
        final String line = lnr.readLine();
        System.out.println( line );
        if( line == null )
          break;

        processLine( line );

        ProgressUtilities.worked( monitor, 1 );
      }

      return StatusUtilities.createStatus( IStatus.OK, ISimulation1D2DConstants.CODE_RMA10S, "Iterations-Log wurde gelesen.", null );
    }
    catch( final CoreException e )
    {
      return e.getStatus();
    }
    catch( final IOException e )
    {
      if( lnr == null )
        return StatusUtilities.createStatus( IStatus.WARNING, ISimulation1D2DConstants.CODE_RMA10S, "Iterations-Log konnte nicht geöffnet werden.", e );

      final String msg = String.format( "Fehler in Zeile %d beim Lesen des Iterations-Log konnte nicht gelesen werden.", lnr.getLineNumber() );
      return StatusUtilities.createStatus( IStatus.WARNING, ISimulation1D2DConstants.CODE_RMA10S, msg, e );
    }
  }

  private void processLine( final String line )
  {
    System.out.println( line );

    final String[] strings = line.trim().split( "\\s+" );

    System.out.println( strings.length );

    if( strings.length != 25 )
      return;

    try
    {
      final BigDecimal[] values = new BigDecimal[strings.length];
      for( int i = 0; i < strings.length; i++ )
        values[i] = new BigDecimal( strings[i].trim() );

      m_stepNr = values[1].intValueExact();

      /* TODO: add all value to this step */
    }
    catch( final NumberFormatException e )
    {
      // at the moment ignored, as this always happens for the first line...
      // TODO: change calc core to produce better output
    }

  }

}
