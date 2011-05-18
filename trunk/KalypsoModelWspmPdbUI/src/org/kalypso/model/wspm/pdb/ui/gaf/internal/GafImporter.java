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
package org.kalypso.model.wspm.pdb.ui.gaf.internal;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.db.PdbState;
import org.kalypso.model.wspm.pdb.db.PdbWaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;

/**
 * @author Gernot Belger
 */
public class GafImporter implements ICoreRunnableWithProgress
{
  private final PrintWriter m_logWriter;

  private final ImportGafData m_data;

  private final IPdbConnection m_connection;

  public GafImporter( final PrintWriter logWriter, final ImportGafData data, final IPdbConnection connection )
  {
    m_logWriter = logWriter;
    m_data = data;
    m_connection = connection;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Read GAF file", 100 );

    GafReader gafReader = null;
    try
    {
      monitor.subTask( "create new state" );
      final PdbState state = addState();
      ProgressUtilities.worked( monitor, 5 );

      monitor.subTask( "find water body" );
      final PdbWaterBody waterBody = findWaterBody();
      ProgressUtilities.worked( monitor, 5 );

      final File gafFile = m_data.getGafFile();
      gafReader = new GafReader( state, waterBody, m_logWriter );
      gafReader.read( gafFile, new SubProgressMonitor( monitor, 90 ) );
      gafReader.close();

      return new Status( IStatus.OK, WspmPdbUiPlugin.PLUGIN_ID, "Successfully imported GAF file" );
    }
    catch( final IOException e )
    {
      final String message = "Error while reading file";
      m_logWriter.println( message );
      e.printStackTrace( m_logWriter );
      return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, message, e );
    }
    finally
    {
      if( gafReader != null )
        gafReader.closeQuietly();
    }
  }

  private PdbState addState( )
  {
    // FIXME
    // TODO Auto-generated method stub

    return m_data.getState();
  }

  private PdbWaterBody findWaterBody( )
  {
    // FIXME
    return null;
  }
}
