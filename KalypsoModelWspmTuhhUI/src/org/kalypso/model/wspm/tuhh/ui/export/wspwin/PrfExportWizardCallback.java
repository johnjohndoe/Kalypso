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
package org.kalypso.model.wspm.tuhh.ui.export.wspwin;

import java.io.File;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.core.profil.IProfil;

public class PrfExportWizardCallback implements IPrfExporterCallback
{
  private final Set<String> m_filenames = new HashSet<String>();

  private final File m_exportDirectory;

  private final String m_filenamePattern;

  public PrfExportWizardCallback( final File exportDir, final String filenamePattern )
  {
    m_exportDirectory = exportDir;
    m_filenamePattern = filenamePattern;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.export.IPrfExporterCallback#getExportFile(org.kalypso.model.wspm.core.profil.IProfil)
   */
  @Override
  public File getExportFile( final IProfil profil )
  {
    final String fileName = ProfilePatternInputReplacer.getINSTANCE().replaceTokens( m_filenamePattern, profil );
    final String uniqueFileName = createUniqueFilename( fileName );
    final String cleanFileName = cleanupFilename( uniqueFileName );
    return new File( m_exportDirectory, cleanFileName + ".prf" );
  }

  private String createUniqueFilename( final String fileName )
  {
    String uniqueName = fileName;

    for( int i = 0; i < Integer.MAX_VALUE && m_filenames.contains( uniqueName ); i++ )
      uniqueName = fileName + i;

    m_filenames.add( uniqueName );
    return uniqueName;
  }

  private String cleanupFilename( final String fileName )
  {
    String result = fileName;
    result = fileName.replace( '#', '_' );
    result = fileName.replace( ':', '_' );
    result = fileName.replace( ' ', '_' );
    result = fileName.replace( ' ', '_' );
    return result;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.export.IPrfExporterCallback#profileWritten(java.io.File)
   */
  @SuppressWarnings("unused")
  @Override
  public void profileWritten( final File file ) throws CoreException
  {
  }

}
