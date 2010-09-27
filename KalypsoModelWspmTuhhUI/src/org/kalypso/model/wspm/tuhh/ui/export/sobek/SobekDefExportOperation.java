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
package org.kalypso.model.wspm.tuhh.ui.export.sobek;

import java.io.File;
import java.util.Formatter;
import java.util.Locale;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.export.wspwin.ProfilePatternInputReplacer;
import org.kalypso.observation.result.IRecord;

/**
 * @author Gernot Belger
 */
public class SobekDefExportOperation extends AbstractSobekProfileExportOperation
{
  private final String m_idPattern;

  public SobekDefExportOperation( final File targetFile, final IProfileFeature[] profilesToExport, final String idPattern )
  {
    super( targetFile, profilesToExport );

    m_idPattern = idPattern;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.export.sobek.ISobekProfileExportOperation#getLabel()
   */
  @Override
  public String getLabel( )
  {
    return ".def Datei";
  }

  @Override
  protected void writeProfile( final Formatter formatter, final IProfil profil )
  {
    final IRecord[] points = getPointsToExport( profil );
    if( points == null )
      return;

    final String id = ProfilePatternInputReplacer.getINSTANCE().replaceTokens( m_idPattern, profil );
    final String profileName = profil.getName();

    formatter.format( "CRDS id '%s' nm '%s' ty 10 st 0 lt sw 0 0 gl 0 gu 0 lt yz%n", id, profileName ); //$NON-NLS-1$
    formatter.format( "TBLE%n" ); //$NON-NLS-1$

    final int widhtIndex = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final int heightIndex = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE );

    for( final IRecord point : points )
    {
      final Number y = (Number) point.getValue( widhtIndex );
      final Number z = (Number) point.getValue( heightIndex );
      formatter.format( Locale.US, "%.4f %.4f <%n", y, z ); //$NON-NLS-1$
    }
    formatter.format( "tble%n" ); //$NON-NLS-1$
    formatter.format( "crds%n" ); //$NON-NLS-1$
  }

  private IRecord[] getPointsToExport( final IProfil profil )
  {
    // TODO: let user choose get marker type
    final String pointMarkerId = IWspmTuhhConstants.MARKER_TYP_BORDVOLL;
    // TODO: get from some registry
    final String markerLabel = "Bordvollpunkte";

    if( pointMarkerId == null )
      return profil.getPoints();

    final IProfilPointMarker[] markers = profil.getPointMarkerFor( pointMarkerId );
    if( markers.length < 2 )
    {
      final String message = String.format( "Gew‰hlte Markierung (%s) bei Profil %.4f (%s) nicht gesetzt. Es wurden alle Profilpunkte exportiert.", markerLabel, profil.getStation(), profil.getName() );
      final IStatus status = new Status( IStatus.WARNING, KalypsoModelWspmTuhhUIPlugin.getID(), message );
      add( status );
      return profil.getPoints();
    }

    final IRecord startPoint = markers[0].getPoint();
    final IRecord endPoint = markers[1].getPoint();

    final int startIndex = profil.indexOfPoint( startPoint );
    final int endIndex = profil.indexOfPoint( endPoint );

    return profil.getPoints( startIndex, endIndex );
  }
}
