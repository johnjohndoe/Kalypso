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
package org.kalypso.model.wspm.pdb.gaf.internal;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.hibernate.Session;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.Executor;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.gaf.ImportGafData;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;

/**
 * @author Gernot Belger
 */
public class GafImporter implements ICoreRunnableWithProgress
{
  private final GafLogger m_logger;

  private final File m_gafFile;

  private final ImportGafData m_data;

  public GafImporter( final File gafFile, final GafLogger logger, final ImportGafData data )
  {
    m_gafFile = gafFile;
    m_logger = logger;
    m_data = data;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final String taskName = String.format( "Reading %s", m_gafFile.getName() );
    monitor.beginTask( taskName, 100 );

    try
    {
      final GafProfile[] profiles = readGaf( new SubProgressMonitor( monitor, 50 ) );
      importGaf( profiles, new SubProgressMonitor( monitor, 50 ) );

      return new Status( IStatus.OK, WspmPdbCorePlugin.PLUGIN_ID, "Successfully imported GAF file" );
    }
    finally
    {
      ProgressUtilities.done( monitor );
    }
  }

  private GafProfile[] readGaf( final IProgressMonitor monitor ) throws CoreException
  {
    GafReader gafReader = null;
    try
    {
      final int srid = m_data.getSrid();

      gafReader = new GafReader( m_logger, srid );
      gafReader.read( m_gafFile, monitor );
      gafReader.close();
      return gafReader.getProfiles();
    }
    catch( final IOException e )
    {
      final String message = "Error while reading file";
      m_logger.log( IStatus.ERROR, message, null, e );
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, message, e );
      throw new CoreException( status );
    }
    finally
    {
      if( gafReader != null )
        gafReader.closeQuietly();
    }
  }

  private void importGaf( final GafProfile[] profiles, final IProgressMonitor monitor ) throws CoreException
  {
    Session session = null;
    try
    {
      session = m_data.getConnection().openSession();
      final State state = m_data.getState();
      final WaterBody waterBody = m_data.getWaterBody();
      final int srid = m_data.getSrid();

      final Gaf2Db gaf2db = new Gaf2Db( waterBody, state, profiles, srid, monitor );
      new Executor( session, gaf2db ).execute();

      session.close();
    }
    catch( final PdbConnectException e )
    {
      final String message = "Failed to write data into database";
      m_logger.log( IStatus.ERROR, message, null, e );
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, message, e );
      throw new CoreException( status );
    }
    finally
    {
      PdbUtils.closeSessionQuietly( session );
    }
  }
}