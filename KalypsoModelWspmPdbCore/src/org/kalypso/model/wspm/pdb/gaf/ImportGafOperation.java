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
package org.kalypso.model.wspm.pdb.gaf;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.hibernate.Session;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.Executor;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.gaf.Gaf2Db;

/**
 * First stage of gaf importing: open log file, then delegate to next level.
 * 
 * @author Gernot Belger
 */
public class ImportGafOperation implements ICoreRunnableWithProgress
{
  private final ImportGafData m_data;

  public ImportGafOperation( final ImportGafData data )
  {
    m_data = data;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Import GAF", 100 );

    checkPreconditions();

    final IStatusCollector stati = new StatusCollector( WspmPdbCorePlugin.PLUGIN_ID );

    /* Upload gaf data into db */
    final IStatus gaf2dbStatus = doGaf2DB( new SubProgressMonitor( monitor, 90 ) );
    if( gaf2dbStatus.matches( IStatus.ERROR ) )
      return gaf2dbStatus;

    /* Write log with status messages */
    final IStatus logStatus = doWriteLog( new SubProgressMonitor( monitor, 10 ) );
    stati.add( logStatus );

    return stati.asMultiStatusOrOK( "Problems during GAF Import", "GAF Import successfully terminated" );
  }

  // REMARK: extra check necessary here, because (due to validation-binding-problem) in some cases
  // we cannot prohibit page completion although the state name already exists.
  // Just produce a nice message and do not leave the wizard. So the user can change the name without pain.
  private void checkPreconditions( ) throws CoreException
  {
    final State newState = m_data.getState();
    final String newStateName = newState.getName();

    final State[] existingStates = m_data.getExistingStates();
    for( final State state : existingStates )
    {
      if( state.getName().equals( newStateName ) )
      {
        final String message = String.format( "A state with the same name '%s' already exists.\nPlease change the state's name.", newStateName );
        final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, message );
        throw new CoreException( status );
      }
    }
  }

  private IStatus doGaf2DB( final IProgressMonitor monitor ) throws CoreException
  {
    Session session = null;
    try
    {
      final IPdbConnection connection = m_data.getConnection();
      final String dbType = connection.getClass().getName();
      session = connection.openSession();
      final State state = m_data.getState();
      final WaterBody waterBody = m_data.getWaterBody();

      final GafProfiles profiles = m_data.getGafProfiles();

      final Event waterlevelEvent = getWaterlevelEvent();

      final Gaf2Db gaf2db = new Gaf2Db( dbType, waterBody, state, profiles, waterlevelEvent, monitor );
      new Executor( session, gaf2db ).execute();

      session.close();

      return new Status( IStatus.OK, WspmPdbCorePlugin.PLUGIN_ID, "GAF data written successfully into database" );
    }
    catch( final PdbConnectException e )
    {
      final String message = "Failed to write data into database";
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, message, e );
      throw new CoreException( status );
    }
    finally
    {
      ProgressUtilities.done( monitor );
      PdbUtils.closeSessionQuietly( session );
    }
  }

  protected Event getWaterlevelEvent( )
  {
    if( !m_data.getHasWaterlevels() )
      return null;

    final boolean importWaterlevels = m_data.getImportWaterlevels();
    if( !importWaterlevels )
      return null;

    return m_data.getWaterlevelEvent();
  }

  private IStatus doWriteLog( final IProgressMonitor monitor )
  {
    StatusWriter writer = null;
    try
    {
      monitor.beginTask( "Writing log file", 100 );

      final File logFile = m_data.getLogFile();
      if( logFile == null )
        return Status.OK_STATUS;

      final IStatus status = m_data.getGafProfiles().getStatus();
      writer = new StatusWriter( logFile );
      writer.write( status );
      writer.close();

      return Status.OK_STATUS;
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      return new Status( IStatus.WARNING, WspmPdbCorePlugin.PLUGIN_ID, "Failed to write log file", e );
    }
    finally
    {
      monitor.done();
      IOUtils.closeQuietly( writer );
    }
  }
}