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

import java.util.Iterator;
import java.util.TreeMap;
import java.util.Map.Entry;

import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.grid.GeoGridUtilities.Interpolation;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IllegalProfileOperationException;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.util.DouglasPeuckerHelper;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * This class should help handling coverages and profiles.
 * 
 * @author Holger Albert
 * @author kimwerner
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
   * <li>the coverage is not set (=null) OR the line does not intersect with any data of the coverage (all line points
   * have resulting NaN-values):
   * <ol>
   * <li>the line is converted into a profile with '0.0' as elevation
   * </ol>
   * <li>coverage is set:
   * <ol>
   * <li>Adds points to the geometry, depending on the size of the grid cell (1/8 grid cell size).</li>
   * <li>Computes the width and height for each point.</li>
   * <li>Create a profile with each point and its width and height (only points with elevation are being considered!)
   * </li>
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
    /* Convert to a JTS geometry. */
    final LineString jtsCurve = (LineString) JTSAdapter.export( curve );

    if( m_coverage == null )
    {
      return ProfilUtil.convertLinestringToEmptyProfile( jtsCurve, m_type );
    }
    else
    {
      final IGeoGrid grid = GeoGridUtilities.toGrid( m_coverage );
      double x = grid.getOffsetX().x;

      /* STEP 1: Add the points every 1m to the profile. */

      /* Add every 1/8 raster size a point. */
      TreeMap<Double, Point> points = JTSUtilities.calculatePointsOnLine( jtsCurve, x / 8 );

      /* STEP 2: Compute the width and height for each point of the new line. */
      /* STEP 3: Create the new profile. */
      IProfil profile = calculatePointsAndCreateProfile( grid, points, curve.getCoordinateSystem() );

      if( profile.getPoints().length == 0 )
      {
        /* in this case the curve does not intersect the grid (all z-values of the points of the curve are NaN) */
        return ProfilUtil.convertLinestringToEmptyProfile( jtsCurve, m_type );
      }
      else
      {
        /* STEP 4: Thin the profile. */
        thinProfile( profile, 0.10 );
      }

      if( grid != null )
        grid.dispose();

      return profile;
    }
  }

  /**
   * This function calculates the points for the profile and creates the new profile.
   * 
   * @param grid
   *            The grid.
   * @param points
   *            All points of the new geo line.
   * @param csOfPoints
   *            The coordinate system of the points.
   * @return The new profile.
   */
  private IProfil calculatePointsAndCreateProfile( IGeoGrid grid, TreeMap<Double, Point> points, String csOfPoints ) throws Exception
  {
    /* Create the new profile. */
    IProfil profile = ProfilFactory.createProfil( m_type );

    /* The needed components. */
    IComponent cRechtswert = profile.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    IComponent cHochwert = profile.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    IComponent cBreite = profile.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_BREITE );
    IComponent cHoehe = profile.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_HOEHE );

    /* add components if necessary */
    if( !profile.hasPointProperty( cRechtswert ) )
      profile.addPointProperty( cRechtswert );
    if( !profile.hasPointProperty( cHochwert ) )
      profile.addPointProperty( cHochwert );
    if( !profile.hasPointProperty( cBreite ) )
      profile.addPointProperty( cBreite );
    if( !profile.hasPointProperty( cHoehe ) )
      profile.addPointProperty( cHoehe );

    /* get index for component */
    final int iRechtswert = profile.indexOfProperty( cRechtswert );
    final int iHochwert = profile.indexOfProperty( cHochwert );
    final int iBreite = profile.indexOfProperty( cBreite );
    final int iHoehe = profile.indexOfProperty( cHoehe );

    /* Iterate over all points in the curve. */
    Iterator<Entry<Double, Point>> iterator = points.entrySet().iterator();
    while( iterator.hasNext() )
    {
      /* Get the current entry set. */
      Entry<Double, Point> entry = iterator.next();

      Double distance = entry.getKey();
      Point point = entry.getValue();

      /* Get grid value. */
      Coordinate coordinate = point.getCoordinate();

      /* Transform the coordinate into the CS of the grid. */
      Coordinate transformedCoordinate = GeoGridUtilities.transformCoordinate( grid, coordinate, csOfPoints );
      /* Get the interpolated value with the coordinate in the coordinate system of the grid. */
      double value = GeoGridUtilities.getValue( grid, transformedCoordinate, Interpolation.bilinear );
      if( Double.isNaN( value ) )
        continue;

      /* All necessary values. */
      double rechtswert = coordinate.x;
      double hochwert = coordinate.y;
      double breite = distance;
      double hoehe = value;

      /* Create a new profile point. */
      IRecord profilePoint = profile.createProfilPoint();

      /* Add geo values. */
      profilePoint.setValue( iRechtswert, rechtswert );
      profilePoint.setValue( iHochwert, hochwert );

      /* Add length section values. */
      profilePoint.setValue( iBreite, breite );
      profilePoint.setValue( iHoehe, hoehe );

      /* Add the new point to the profile. */
      profile.addPoint( profilePoint );
    }

    return profile;
  }

  /**
   * This function thins the profile and removes uneccessary points. It uses the Douglas Peucker algorythm.
   * 
   * @param profile
   *            The profile, which should be thinned.
   * @param allowedDistance
   *            The allowed distance [m].
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