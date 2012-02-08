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
package org.kalypso.model.wspm.tuhh.ui.export.bankline;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.ui.export.bankline.BanklineDistanceBuilder.SIDE;
import org.kalypso.transformation.transformer.JTSTransformer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implementation of {@link IBanklineMarkerProvider} that returns the positions of markers of a certain type.
 *
 * @author Gernot Belger
 */
public class BanklineMarkerProvider implements IBanklineMarkerProvider
{
  private final String m_markerTyp;

  private final String m_markerTypeFallback;

  private final String m_label;

  /**
   * @param markerType
   *          The type of the marker that will be returned.
   * @param markerTypeFallback
   *          Used, if no marker of the standard type is found. If <code>null</code>, the end points of the cross
   *          section are used as fallback.
   */
  public BanklineMarkerProvider( final String label, final String markerTyp, final String markerTypeFallback )
  {
    m_label = label;
    m_markerTyp = markerTyp;
    m_markerTypeFallback = markerTypeFallback;
  }

  @Override
  public String getId( )
  {
    final String className = getClass().getSimpleName();
    return String.format( "%s: %s (%s)", className, m_markerTyp, m_markerTypeFallback ); //$NON-NLS-1$
  }

  @Override
  public String toString( )
  {
    return m_label;
  }

  @Override
  public Coordinate getMarkerLocation( final String profileSRS, final IProfil profile, final SIDE side ) throws Exception
  {
    final String kalypsoSRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    final int profileSRID = JTSAdapter.toSrid( profileSRS );
    final int kalypsoSRID = JTSAdapter.toSrid( kalypsoSRS );

    final IProfileRecord point = findMarkerPoint( profile, side );

    final JTSTransformer jtsTransformer = new JTSTransformer( profileSRID, kalypsoSRID );
    return jtsTransformer.transform( point.getCoordinate() );
  }

  private IProfileRecord findMarkerPoint( final IProfil profile, final SIDE side )
  {
    final IProfileRecord primaryResult = doFindMarkerPoint( profile, m_markerTyp, side );
    if( primaryResult != null )
      return primaryResult;

    final IProfileRecord secondaryResult = doFindMarkerPoint( profile, m_markerTypeFallback, side );
    if( secondaryResult != null )
      return secondaryResult;

    /* always last resort: use profile end points */
    return doFindMarkerPoint( profile, null, side );
  }

  private IProfileRecord doFindMarkerPoint( final IProfil profile, final String markerType, final SIDE side )
  {
    final IProfileRecord[] markers = findMarkersOfType( profile, markerType );
    if( markers.length < 2 )
      return null;

    switch( side )
    {
      case left:
        return markers[0];

      case right:
      default:
        return markers[markers.length - 1];
    }
  }

  private IProfileRecord[] findMarkersOfType( final IProfil profile, final String markerType )
  {
    if( markerType == null )
      return profile.getPoints();

    final IProfilPointMarker[] pointMarkers = profile.getPointMarkerFor( markerType );
    final IProfileRecord[] points = new IProfileRecord[pointMarkers.length];
    for( int i = 0; i < points.length; i++ )
      points[i] = pointMarkers[i].getPoint();
    return points;
  }
}