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
package org.kalypso.model.wspm.tuhh.core.results.processing;

import java.io.File;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class ResultLSTableFile extends AbstractResultLSFile
{
  private final String m_dataFilename;

  public ResultLSTableFile( final File outDir, final String runoffName, final String dataFilename )
  {
    super( outDir, runoffName );
    m_dataFilename = dataFilename;
  }

  @Override
  public String getFilename( )
  {
    return "Tabelle" + getRunoffName() + ".gft"; //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public String getTitle( )
  {
    return Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.27" ); //$NON-NLS-1$ 
  }

  @Override
  public String getResultID( )
  {
    return "LengthSectionTab"; //$NON-NLS-1$ 
  }

  @Override
  protected void doWrite( final File outputFile ) throws Exception
  {
    final URL tableUrl = getClass().getResource( "resources/table.gft" ); //$NON-NLS-1$
    final String tableTemplate = FileUtilities.toString( tableUrl, "UTF-8" ); //$NON-NLS-1$
    final String table = tableTemplate.replaceAll( ResultLSChartFile.TOKEN_GMLFILENAME, m_dataFilename );
    FileUtils.writeStringToFile( outputFile, table, "UTF-8" ); //$NON-NLS-1$
  }
}
