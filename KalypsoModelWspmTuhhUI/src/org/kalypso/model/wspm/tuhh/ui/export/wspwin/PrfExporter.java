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

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfile;
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
    final MultiStatus resultStatus = new MultiStatus( id, 1, Messages.getString( "PrfExporter_0" ), null ); //$NON-NLS-1$

    monitor.beginTask( Messages.getString( "PrfExporter_1" ), profiles.length ); //$NON-NLS-1$

    for( final IProfileFeature feature : profiles )
    {
      final IProfile profil = feature.getProfile();
      final String profileName = profil.getName();
      final double station = profil.getStation();
      final WspmWaterBody water = feature.getWater();

      monitor.subTask( String.format( Messages.getString( "PrfExporter_2" ), profileName, station ) ); //$NON-NLS-1$

      final File file = m_callback.getExportFile( feature, profil );
      if( file.exists() )
      {
        System.out.println( Messages.getString( "PrfExporter_3" ) + file ); //$NON-NLS-1$
      }

      try
      {
        // TODO: distinguish between waterlevel fixations and waterlevels
        final IWaterlevel[] waterlevels = m_callback.getWaterlevels( profil );

        final String defaultRoughnessType = m_callback.getDefaultRoughnessType();
        final boolean preferRoughnessClasses = m_callback.getPreferRoughnessClasses();
        final boolean preferVegetationClasses = m_callback.getPreferVegetationClasses();

        final PrfWriter prfWriter = new PrfWriter( profil, waterlevels, defaultRoughnessType, preferRoughnessClasses, preferVegetationClasses );

        configurePrfWriterWithMetadata( water, profil, prfWriter );

        // WspWin needs the profile number for import. We are using the station in [m] to ao avoid conflicts
        final int profileNumber = (int) (station * 1000.0);
        prfWriter.setPrfMetadata( IPrfConstants.PRF_LINE_8_BLATTBEZEICHNUNG_2, Integer.toString( profileNumber ) );

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
  public static void configurePrfWriterWithMetadata( final WspmWaterBody water, final IProfile profil, final PrfWriter prfWriter )
  {
    final String profileName = profil.getName();
    final String profileDescription = profil.getDescription();
    final String riverId = water == null ? StringUtils.EMPTY : water.getRefNr();
    final String riverDescription = water == null ? StringUtils.EMPTY : water.getDescription();
    prfWriter.setPrfMetadata( IPrfConstants.PRF_LINE_4_PROJEKTBEZEICHNUNG_1, riverId );
    prfWriter.setPrfMetadata( IPrfConstants.PRF_LINE_5_PROJEKTBEZEICHNUNG_2, profileDescription );
    prfWriter.setPrfMetadata( IPrfConstants.PRF_LINE_6_PROJEKTBEZEICHNUNG_3, riverDescription );
    prfWriter.setPrfMetadata( IPrfConstants.PRF_LINE_13_ZEICHNUNGSUEBERSCHRIFT, profileName );
  }
}