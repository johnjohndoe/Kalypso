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
package org.kalypso.model.wspm.tuhh.schema.simulation;

import java.io.File;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.tuhh.schema.KalypsoModelWspmTuhhSchemaPlugin;
import org.kalypso.model.wspm.tuhh.schema.i18n.Messages;

/**
 * @author Gernot Belger
 */
public abstract class AbstractResultLSFile implements IResultLSFile
{
  private final String m_runoffName;

  private final File m_outDir;

  public AbstractResultLSFile( final File outDir, final String runoffName )
  {
    m_outDir = outDir;
    m_runoffName = runoffName;
  }

  protected String getRunoffName( )
  {
    return m_runoffName;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.schema.simulation.IResultLSFile#getResultFile()
   */
  @Override
  public final File getResultFile( )
  {
    return new File( m_outDir, getFilename() );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.schema.simulation.IResultLSFile#writeFile()
   */
  @Override
  public final IStatus writeFile( )
  {
    try
    {
      final File resultFile = getResultFile();
      doWrite( resultFile );
      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      final String title = getTitle();
      final String message = String.format( Messages.getString("AbstractResultLSFile_0"), title ); //$NON-NLS-1$
      return new Status( IStatus.ERROR, KalypsoModelWspmTuhhSchemaPlugin.getID(), message, e );
    }
  }

  protected abstract void doWrite( File outputFile ) throws Exception;
}
