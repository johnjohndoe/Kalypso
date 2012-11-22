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
package org.kalypso.kalypsomodel1d2d.ui.map.dikeditchgen;

import org.kalypsodeegree.model.elevation.IElevationModel;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateFilter;

/**
 * @author kurzbach
 */
public class InterpolateElevationFilter implements CoordinateFilter
{
  private final String m_coordinateSystem;

  private final IElevationModel m_elevationModel;

  private final double m_minimumHeight;

  public InterpolateElevationFilter( final String pointCoordinateSystem, final IElevationModel elevationModel )
  {
    this( pointCoordinateSystem, Double.NEGATIVE_INFINITY, elevationModel );
  }

  public InterpolateElevationFilter( final String pointCoordinateSystem, final double minimumHeight, final IElevationModel elevationModel )
  {
    m_coordinateSystem = pointCoordinateSystem;
    m_elevationModel = elevationModel;
    m_minimumHeight = minimumHeight;
  }

  @Override
  public void filter( final Coordinate coord )
  {
    try
    {
      // interpret coordinate in its own coordinate system
      final GM_Point p = GeometryFactory.createGM_Point( JTSAdapter.wrap( coord ), m_coordinateSystem );
      double currentElevation = m_elevationModel.getElevation( p );
      if( currentElevation < m_minimumHeight )
        currentElevation = m_minimumHeight;
      coord.z = currentElevation;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }
}