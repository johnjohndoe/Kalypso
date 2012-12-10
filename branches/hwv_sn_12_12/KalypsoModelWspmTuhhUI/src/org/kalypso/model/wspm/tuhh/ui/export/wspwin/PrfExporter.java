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
import java.io.IOException;

import org.eclipse.core.runtime.CoreException;
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
import org.kalypso.model.wspm.tuhh.core.wspwin.prf.IWaterlevel;
import org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfWriter;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;

/**
 * @author Gernot Belger
 */
public class PrfExporter
{
  private final IPrfExporterCallback m_callback;

  public PrfExporter( final IPrfExporterCallback callback )
  {
    m_callback = callback;
  }

  public IStatus export( final IProfileFeature[] profiles, final IProgressMonitor monitor )
  {
    final String id = PluginUtilities.id( KalypsoModelWspmUIPlugin.getDefault() );
    final MultiStatus resultStatus = new MultiStatus( id, 1, Messages.getString("PrfExporter_0"), null ); //$NON-NLS-1$

    monitor.beginTask( Messages.getString("PrfExporter_1"), profiles.length ); //$NON-NLS-1$

    for( final IProfileFeature feature : profiles )
    {
      final IProfil profil = feature.getProfil();
      final String profileName = profil.getName();
      final double station = profil.getStation();
      final WspmWaterBody water = feature.getWater();

      monitor.subTask( String.format( Messages.getString("PrfExporter_2"), profileName, station ) ); //$NON-NLS-1$

      final File file = m_callback.getExportFile( feature, profil );
      if( file.exists() )
      {
        System.out.println( Messages.getString("PrfExporter_3") + file ); //$NON-NLS-1$
      }

      try
      {
        final IWaterlevel[] waterlevels = m_callback.getWaterlevels( profil );
        final PrfWriter prfWriter = new PrfWriter( profil, waterlevels );

        configurePrfWriterWithMetadata( water, profil, prfWriter );

        prfWriter.write( file );

        m_callback.profileWritten( file );
      }
      catch( final IOException e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        resultStatus.add( status );
      }
      catch( final CoreException e )
      {
        resultStatus.add( e.getStatus() );
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
}
