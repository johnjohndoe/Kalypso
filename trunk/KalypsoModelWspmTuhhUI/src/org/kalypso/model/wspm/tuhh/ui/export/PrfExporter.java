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

  public PrfExporter( final File exportDirectory )
  {
    m_exportDirectory = exportDirectory;
  }

  public IStatus export( final IProfileFeature[] profiles, final IProgressMonitor monitor )
  {
    final String id = PluginUtilities.id( KalypsoModelWspmUIPlugin.getDefault() );
    final MultiStatus resultStatus = new MultiStatus( id, 1, "Several profiles could not be written.", null );

    monitor.beginTask( "Profiles are being saved -", profiles.length );

    for( final IProfileFeature feature : profiles )
    {
      final IProfil profil = feature.getProfil();
      final String profileName = profil.getName();
      final WspmWaterBody water = feature.getWater();

      monitor.subTask( profileName );

      final String fileName = createWspWinFileName( profil );
      final String cleanFileName = cleanupFilename( fileName );
      final File file = new File( m_exportDirectory, cleanFileName );

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

  // FIXME: where to get this flag from...?
  private final static boolean wspwinFileNames = false;

  public static String createWspWinFileName( final IProfil profile )
  {
    if( wspwinFileNames )
    {
      final double station = profile.getStation();
      final String stationString = String.format( "%.4f", station ).replace( '.', '+' ).replace( ' ', '0' );

      return String.format( "%s.prf", stationString );
    }
    else
      return profile.getName();
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
