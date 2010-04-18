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
package org.kalypso.model.wspm.tuhh.ui.export;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.wspwin.prf.IPrfConstants;
import org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfWriter;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;

/**
 * @author Gernot Belger
 */
public class PrfExporter
{
  private final File m_exportDirectory;

  private final Set<String> m_filenames = new HashSet<String>();

  private final String m_filenamePattern;

  public PrfExporter( final File exportDirectory, final String filenamePattern )
  {
    m_exportDirectory = exportDirectory;
    m_filenamePattern = filenamePattern;
  }

  public IStatus export( final IProfileFeature[] profiles, final IProgressMonitor monitor )
  {
    final String id = PluginUtilities.id( KalypsoModelWspmUIPlugin.getDefault() );
    final MultiStatus resultStatus = new MultiStatus( id, 1, "Several profiles could not be written.", null );

    monitor.beginTask( "Profiles are being saved", profiles.length );

    for( final IProfileFeature feature : profiles )
    {
      final IProfil profil = feature.getProfil();
      final String profileName = profil.getName();
      final double station = profil.getStation();
      final WspmWaterBody water = feature.getWater();

      monitor.subTask( String.format( "%s (km %.4f)", profileName, station ) );

      final String fileName = ProfilePatternInputReplacer.getINSTANCE().replaceTokens( m_filenamePattern, profil );
      final String uniqueFileName = createUniqueFilename( fileName );
      final String cleanFileName = cleanupFilename( uniqueFileName );
      final File file = new File( m_exportDirectory, cleanFileName + ".prf" );

      if( file.exists() )
      {
        System.out.println( "File already exists: " + file );
      }

      try
      {
        final PrfWriter prfWriter = new PrfWriter( profil );

        configurePrfWriterWithMetadata( water, profil, prfWriter );

        prfWriter.write( file );
      }
      catch( final IOException e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        resultStatus.add( status );
      }

      monitor.worked( 1 );
      if( monitor.isCanceled() )
        return new Status( IStatus.CANCEL, id, 1, "Operation cancelled by user", null ); //$NON-NLS-1$
    }
    return resultStatus;
  }

  private String createUniqueFilename( final String fileName )
  {
    String uniqueName = fileName;

    for( int i = 0; i < Integer.MAX_VALUE && m_filenames.contains( uniqueName ); i++ )
      uniqueName = fileName + i;

    m_filenames.add( uniqueName );
    return uniqueName;
  }

  // FIXME: get this information from a wizard page (let the user define what to do)
  // Let all prf-exports use the same wizard page.
  public static void configurePrfWriterWithMetadata( final WspmWaterBody water, final IProfil profil, final PrfWriter prfWriter )
  {
    final String profileName = profil.getName();
    final String riverId = water.getRefNr();
    final String riverDescription = water.getDescription();
    final String riverDescriptionCleaned = riverDescription.replace( '\n', '-' ).replace( '\r', '-' );
    prfWriter.setPrfMetadata( IPrfConstants.PRF_LINE_4_PROJEKTBEZEICHNUNG_1, riverId );
    prfWriter.setPrfMetadata( IPrfConstants.PRF_LINE_6_PROJEKTBEZEICHNUNG_3, riverDescriptionCleaned );
    prfWriter.setPrfMetadata( IPrfConstants.PRF_LINE_13_ZEICHNUNGSUEBERSCHRIFT, profileName );
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

}
