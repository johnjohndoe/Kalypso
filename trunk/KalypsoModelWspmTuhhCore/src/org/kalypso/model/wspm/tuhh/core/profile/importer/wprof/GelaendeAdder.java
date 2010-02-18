/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.tuhh.core.profile.importer.wprof;

import java.math.BigDecimal;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint;
import org.kalypso.observation.result.IRecord;
import org.kalypso.transformation.GeoTransformer;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger
 */
class GelaendeAdder implements IProfileDataAdder, IWspmTuhhConstants
{
  private final IWProfPoint[] m_soilPoints;

  public GelaendeAdder( final IWProfPoint[] soilPolygon )
  {
    m_soilPoints = soilPolygon;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.wprof.IProfileDataAdder#configure(org.kalypso.model.wspm.core.profil.IProfil,
   *      org.kalypso.transformation.GeoTransformer)
   */
  @Override
  public void configure( final IProfil profile, final GeoTransformer transformer ) throws CoreException
  {
    try
    {
      final int hoeheIndex = profile.indexOfProperty( POINT_PROPERTY_HOEHE );
      final int rwIndex = profile.indexOfProperty( POINT_PROPERTY_RECHTSWERT );
      final int hwIndex = profile.indexOfProperty( POINT_PROPERTY_HOCHWERT );
      final int commentIndex = profile.indexOfProperty( POINT_PROPERTY_COMMENT );

      for( final IWProfPoint wprofPoint : m_soilPoints )
      {
        final BigDecimal distance = wprofPoint.getDistance();
        final double value = wprofPoint.getValue();
        final GM_Point location = wprofPoint.getLocation();
        final String comment = wprofPoint.getComment();

        final IRecord point = createPoint( profile, distance );
        point.setValue( hoeheIndex, value );

        if( location != null )
        {
          final GM_Point transformedLocation = (GM_Point) transformer.transform( location );
          point.setValue( rwIndex, transformedLocation.getX() );
          point.setValue( hwIndex, transformedLocation.getY() );
        }

        // for( final String markerID : markerIDs )
        // {
        // final IProfilPointMarker marker = profil.createPointMarker( markerID, point );
        // final Object defaultValue = provider.getDefaultValue( markerID );
        // marker.setValue( defaultValue );
        // }

        if( comment != null && !comment.isEmpty() )
          point.setValue( commentIndex, comment );
      }
    }
    catch( final Exception e )
    {
      final String message = String.format( "Unable to create profile at %.4f", profile.getStation() ); //$NON-NLS-1$
      final Status status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhCorePlugin.getID(), message, e );
      throw new CoreException( status );
    }
  }

  private IRecord createPoint( final IProfil profil, final BigDecimal distance )
  {
    final int indexOfDistance = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );

    // Höhe values always get added as new points; we assume that the points are in the right order
    // This is necessary the preserve 'Rücksprünge' in the soil-layer
    final IRecord newPoint = profil.createProfilPoint();
    newPoint.setValue( indexOfDistance, new Double( distance.doubleValue() ) );
    profil.addPoint( newPoint );
    return newPoint;

  }

}
