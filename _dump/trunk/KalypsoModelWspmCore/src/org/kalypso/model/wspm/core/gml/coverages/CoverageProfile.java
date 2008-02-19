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
package org.kalypso.model.wspm.core.gml.coverages;

import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IllegalProfileOperationException;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.util.DouglasPeuckerHelper;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * This class should help handling coverages and profiles.
 * 
 * @author Holger Albert
 */
public class CoverageProfile
{
  /**
   * The coverage.
   */
  private ICoverage m_coverage;

  /**
   * The profile type.
   */
  private String m_type;

  /**
   * The constructor.
   * 
   * @param coverage
   *            The coverage.
   * @param type
   *            The profile type, which should be created.
   */
  public CoverageProfile( ICoverage coverage, String type )
  {
    m_coverage = coverage;
    m_type = type;
  }

  /**
   * This function creates a new profile.<br>
   * <br>
   * The following steps are performed:<br>
   * <ol>
   * <li>Adds points to the geometry, so that there is at least one every 1m.</li>
   * <li>Computes the width and height for each point.</li>
   * <li>Create a profile with each point and its width and height.</li>
   * <li>The new profile is thinned by Douglas Peucker.</li>
   * </ol>
   * <br>
   * If you want to thin the profile, use the result and the function {@link #thinProfile(IProfil)}.
   * 
   * @param curve
   *            The curve, which represents the geometry on the map of the profile.
   * @return The new profile.
   */
  public IProfil createProfile( GM_Curve curve ) throws Exception
  {
    /* STEP 1: Add the points every 1m to the profile. */

    /* Convert to a JTS geometry. */
    LineString jtsCurve = (LineString) JTSAdapter.export( curve );

    /* Add every 1m a point. */
    LineString newCurve = JTSUtilities.addPointsToLine( jtsCurve );

    /* STEP 2: Compute the width and height for each point of the new line. */
    /* STEP 3: Create the new profile. */
    IProfil profile = calculatePointsAndCreateProfile( GeoGridUtilities.toGrid( m_coverage ), newCurve );

    /* STEP 4: Thin the profile. */
    thinProfile( profile, 10 );

    return profile;
  }

  /**
   * This function calculates the points for the profile and creates the new profile.
   * 
   * @param grid
   *            The grid.
   * @param curve
   *            The curve, which points will be used as geo coordinates in the profile.
   * @return The new profile.
   */
  private IProfil calculatePointsAndCreateProfile( IGeoGrid grid, LineString curve ) throws Exception
  {
    /* The geometry factory. */
    GeometryFactory factory = new GeometryFactory( curve.getPrecisionModel(), curve.getSRID() );

    /* Create the new profile. */
    IProfil profile = ProfilFactory.createProfil( m_type );

    /* The needed components. */
    IComponent cRechtswert = ProfilObsHelper.getPropertyFromId( profile, IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    IComponent cHochwert = ProfilObsHelper.getPropertyFromId( profile, IWspmConstants.POINT_PROPERTY_HOCHWERT );
    IComponent cBreite = ProfilObsHelper.getPropertyFromId( profile, IWspmConstants.POINT_PROPERTY_BREITE );
    IComponent cHoehe = ProfilObsHelper.getPropertyFromId( profile, IWspmConstants.POINT_PROPERTY_HOEHE );

    /* Iterate over all points in the curve. */
    Coordinate[] coordinates = curve.getCoordinates();
    for( Coordinate coordinate : coordinates )
    {
      /* Get grid value. */
      double value = grid.getValue( coordinate );
      if( value == Double.NaN )
        continue;

      /* All neccessary values. */
      double rechtswert = coordinate.x;
      double hochwert = coordinate.y;
      double breite = JTSUtilities.pointDistanceOnLine( curve, factory.createPoint( coordinate ) );
      double hoehe = value;

      /* Create a new profile point. */
      IRecord point = profile.createProfilPoint();

      /* Add geo values. */
      point.setValue( cRechtswert, rechtswert );
      point.setValue( cHochwert, hochwert );

      /* Add length section values. */
      point.setValue( cBreite, breite );
      point.setValue( cHoehe, hoehe );

      /* Add the new point to the profile. */
      profile.addPoint( point );
    }

    return profile;
  }

  /**
   * This function thins the profile and removes uneccessary points. It uses the Douglas Peucker algorythm.
   * 
   * @param profile
   *            The profile, which should be thinned.
   * @param allowedDistance
   *            The allowed distance.
   */
  private void thinProfile( IProfil profile, double allowedDistance ) throws IllegalProfileOperationException
  {
    /* Get the profile changes. */
    IProfilChange[] removeChanges = DouglasPeuckerHelper.reduce( allowedDistance, profile.getPoints(), profile );
    if( removeChanges.length == 0 )
      return;

    /* Perform the changes. */
    for( IProfilChange profilChange : removeChanges )
      profilChange.doChange( null );
  }

  /**
   * This function returns the coverage.
   * 
   * @return The coverage.
   */
  public ICoverage getCoverage( )
  {
    return m_coverage;
  }

  /**
   * This function sets the coverage.
   * 
   * @param coverage
   *            The coverage.
   */
  public void setCoverage( ICoverage coverage )
  {
    m_coverage = coverage;
  }

  /**
   * This function returns the profile type..
   * 
   * @return The profile type.
   */
  public String getType( )
  {
    return m_type;
  }

  /**
   * This function sets the profile type.
   * 
   * @param type
   *            The profile type.
   */
  public void setType( String type )
  {
    m_type = type;
  }
}