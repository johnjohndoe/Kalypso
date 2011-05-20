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
package org.kalypso.model.wspm.pdb.gaf.internal;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;

/**
 * @author Gernot Belger
 */
public class GafImporter implements ICoreRunnableWithProgress
{
  private final GafLogger m_logger;

  private final Gaf2Db m_gaf2db;

  private final File m_gafFile;

  public GafImporter( final File gafFile, final GafLogger logger, final Gaf2Db gaf2db )
  {
    m_gafFile = gafFile;
    m_logger = logger;
    m_gaf2db = gaf2db;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    monitor.beginTask( "Read GAF file", 100 );

    GafReader gafReader = null;
    try
    {
      m_gaf2db.addState();

      gafReader = new GafReader( m_logger, m_gaf2db );
      gafReader.read( m_gafFile, new SubProgressMonitor( monitor, 90 ) );
      gafReader.close();

      return new Status( IStatus.OK, WspmPdbCorePlugin.PLUGIN_ID, "Successfully imported GAF file" );
    }
    catch( final IOException e )
    {
      final String message = "Error while reading file";
      m_logger.log( IStatus.ERROR, message, null, e );
      return new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, message, e );
    }
    catch( final PdbConnectException e )
    {
      final String message = "Failed to write data into database";
      m_logger.log( IStatus.ERROR, message, null, e );
      return new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, message, e );
    }
    finally
    {
      if( gafReader != null )
        gafReader.closeQuietly();
    }
  }
}
