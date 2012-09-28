/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.imports.ewawi;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.IProfilePointPropertyProvider;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;

/**
 * @author Holger Albert
 */
public class EwawiProfilePointMarkerCreator
{
  private final IProfileFeature m_profileFeature;

  public EwawiProfilePointMarkerCreator( final IProfileFeature profileFeature )
  {
    m_profileFeature = profileFeature;
  }

  public void createProfilePointMarker( )
  {
    /* Get the profile point property provider. */
    final IProfilePointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( IWspmTuhhConstants.PROFIL_TYPE_PASCHE );

    /* Get the profile. */
    final IProfile profil = m_profileFeature.getProfile();

    /* Create the point marker for all points using the code property. */
    createPointMarker( provider, profil );

    /* Create the point marker for the first and last point. */
    final IProfileRecord firstPoint = profil.getFirstPoint();
    final IProfileRecord lastPoint = profil.getLastPoint();
    final IProfilePointMarker firstMarker = profil.createPointMarker( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, firstPoint );
    final IProfilePointMarker lastMarker = profil.createPointMarker( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, lastPoint );
    firstMarker.setValue( provider.getDefaultValue( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
    lastMarker.setValue( provider.getDefaultValue( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
  }

  private void createPointMarker( final IProfilePointPropertyProvider provider, final IProfile profil )
  {
    final IProfileRecord[] points = profil.getPoints();
    for( final IProfileRecord point : points )
    {
      final String code = (String)point.getValue( profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_CODE ) );
      if( code == null || code.length() == 0 )
        continue;

      final String markerID = findMarkerID( code );
      if( markerID != null && markerID.length() > 0 )
      {
        final IProfilePointMarker marker = profil.createPointMarker( markerID, point );
        marker.setValue( provider.getDefaultValue( markerID ) );
      }
    }
  }

  private String findMarkerID( final String code )
  {
    // BUK
    if( code.equals( "EWAWI_8" ) ) //$NON-NLS-1$ 
      return IWspmTuhhConstants.MARKER_TYP_BORDVOLL;

    // BOK
    if( code.equals( "EWAWI_9" ) ) //$NON-NLS-1$
      return IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE;

    return null;
  }
}