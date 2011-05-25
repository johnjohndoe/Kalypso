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
package org.kalypso.model.wspm.pdb.gaf;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.pdb.gaf.internal.GafImporter;
import org.kalypso.model.wspm.pdb.gaf.internal.GafLogger;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;

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
  public IStatus execute( final IProgressMonitor monitor )
  {
    monitor.beginTask( "Import GAF", 100 );

    // Handle log first and separately in order to separate reading errors from log problems
    GafLogger logger = null;

    try
    {
      logger = new GafLogger( m_data.getLogFile() );

      final File gafFile = m_data.getGafFile();
      final GafImporter importer = new GafImporter( gafFile, logger, m_data );
      return importer.execute( monitor );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, "Error while writing log file", e );
    }
    finally
    {
      if( logger != null )
        logger.close();
    }
  }
}