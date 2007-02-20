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
package org.kalypso.model.wspm.tuhh.schema.simulation;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.util.LogHelper;

/**
 * Helper clas to start the processing of the polynomes.
 * 
 * @author Gernot Belger
 */
public class PolynomeHelper
{

  /**
   * Prepares the input files for the polynome process
   * 
   * @param tmpDir
   *          any tmp dir, must be empty before start, may be deleted after end
   * @param dathDir
   *          Directory containing the laengsschnitt.txt and the beiwerte.aus files.
   * @return true, if preparation was succesful
   */
  private static boolean preparePolynomes( final File tmpDir, final File dathDir, final LogHelper log )
  {
    final File lsFile = new File( dathDir, "laengsschnitt.txt" );
    final File ausFile = new File( dathDir, "Beiwerte.AUS" );

    /* Check input data */
    // TODO: comment in
    // if( !lsFile.exists() )
    // {
    // log.log( false, "Ergebnisdatei %s für Polynonerzeugung nicht vorhanden. Abbruch.", lsFile );
    // return false;
    // }
    //
    // if( !ausFile.exists() )
    // {
    // log.log( false, "Ergebnisdatei %s für Polynonerzeugung nicht vorhanden. Abbruch.", ausFile );
    // return false;
    // }
    /* Prepare exe dir */
    InputStream zipInputStream = null;
    try
    {
      zipInputStream = new BufferedInputStream( WspmTuhhCalcJob.class.getResourceAsStream( "resources/polynom1d.zip" ) );
      ZipUtilities.unzip( zipInputStream, tmpDir, false );
      zipInputStream.close();
    }
    catch( final IOException e )
    {
      log.log( e, "Fehler beim Entpacken der polynom1d.exe. Abbruch." );
      return false;
    }
    finally
    {
      IOUtils.closeQuietly( zipInputStream );
    }

    /* Copy input data to exe dir */
    // TODO: comment in
    // try
    // {
    // final File eingangDir = new File( tmpDir, "01Eingang" );
    // FileUtils.copyFileToDirectory( lsFile, eingangDir );
    // FileUtils.copyFileToDirectory( ausFile, eingangDir );
    // }
    // catch( final IOException e )
    // {
    // log.log( e, "Eingangsdaten für Polynomberechnung konnten nicht kopiert werden. Abbruch." );
    // return false;
    // }
    return true;
  }

  public static void processPolynomes( final File tmpDir, final File dathDir, final LogHelper log, final long timeout, final ISimulationResultEater resultEater ) throws SimulationException
  {
    if( !preparePolynomes( tmpDir, dathDir, log ) )
      return;

    final ISimulationMonitor monitor = log.getMonitor();
    if( monitor.isCanceled() )
      return;

    /* Start the polynome1d process */
    final File exeFile = new File( tmpDir, "Polynome1d.exe" );
    final String cmdLine = exeFile.getAbsolutePath();

    final File logFile = new File( tmpDir, "Polynome1d.log" );
    // resultEater.addResult( "Polynome1DLog", logFile );
    final File errFile = new File( tmpDir, "Polynome1d.err" );
    // resultEater.addResult( "Polynome1DErr", errFile );

    OutputStream logStream = null;
    OutputStream errStream = null;
    try
    {
      logStream = new BufferedOutputStream( new FileOutputStream( logFile ) );
      errStream = new BufferedOutputStream( new FileOutputStream( errFile ) );

      // TODO: comment in
      // ProcessHelper.startProcess( cmdLine, null, exeFile.getParentFile(), monitor, timeout, logStream, errStream,
      // null );

      logStream.close();
      errStream.close();
    }
    catch( final IOException e )
    {
      log.log( e, "Fehler bei der Ausführung der Polynome1D.exe: %s" + e.getLocalizedMessage() );
      monitor.setFinishInfo( IStatus.ERROR, "Fehler bei der ausführung der Polynome1D.exe" );
      return;
    }
    // TODO: comment in
    // catch( final ProcessTimeoutException e )
    // {
    // log.log( false, "Polynome1D-Prozess wurde abgebrochen. Grund: timeout" );
    // monitor.setFinishInfo( IStatus.ERROR, "Polynome1D Prozess wurde abgebrochen. Grund: timeout" );
    // return;
    // }
    finally
    {
      IOUtils.closeQuietly( logStream );
      IOUtils.closeQuietly( errStream );
    }

    if( log.checkCanceled() )
      return;

    readResults( monitor );
  }

  private static void readResults( final ISimulationMonitor monitor )
  {
    /* check results */

    /* Convert results to gml */

  }
}
