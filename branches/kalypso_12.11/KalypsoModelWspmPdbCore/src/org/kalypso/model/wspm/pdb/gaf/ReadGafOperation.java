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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.gaf.GafLine;
import org.kalypso.model.wspm.pdb.internal.gaf.GafReader;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;

/**
 * First stage of gaf reading: open log file, then delegate to next level.
 * 
 * @author Gernot Belger
 */
public class ReadGafOperation implements ICoreRunnableWithProgress
{
  private final ImportGafData m_data;

  public ReadGafOperation( final ImportGafData data )
  {
    m_data = data;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    GafReader gafReader = null;
    try
    {
      /* Prepare for exception */
      m_data.setLines( null );

      final File gafFile = m_data.getGafFile();

      gafReader = new GafReader();
      final IStatus status = gafReader.read( gafFile, monitor );
      gafReader.close();

      final GafLine[] gafLines = gafReader.getLines();

      m_data.setReadGafStatus( status );
      m_data.setLines( gafLines );

      return Status.OK_STATUS;
    }
    catch( final IOException e )
    {
      final String message = Messages.getString( "ReadGafOperation.0" ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, message, e );
      throw new CoreException( status );
    }
    finally
    {
      if( gafReader != null )
        gafReader.closeQuietly();
    }
  }
}