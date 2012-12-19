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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypsodeegree.model.elevation.ElevationException;
import org.kalypsodeegree.model.elevation.IElevationModel;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * An {@link IElevationModel} implementation based on an array of inner elevation nmodels.
 *
 * @author Gernot Belger
 */
public class ElevationArrayModel implements IElevationModel
{
  private final ITerrainElevationModel[] m_models;

  public ElevationArrayModel( final ITerrainElevationModel[] models )
  {
    m_models = models;
  }

  @Override
  public void dispose( )
  {
  }

  @Override
  public double getElevation( final GM_Point location ) throws ElevationException
  {
    for( final IElevationModel model : m_models )
    {
      final double elevation = model.getElevation( location );
      if( !Double.isNaN( elevation ) )
        return elevation;
    }

    return Double.NaN;
  }

  @Override
  public GM_Envelope getBoundingBox( ) throws ElevationException
  {
    GM_Envelope box = null;

    for( final IElevationModel model : m_models )
    {
      final GM_Envelope boundingBox = model.getBoundingBox();
      if( box == null )
        box = boundingBox;
      else
        box = box.getMerged( boundingBox );
    }

    return box;
  }

  @Override
  public double getMinElevation( ) throws ElevationException
  {
    double minElevation = Double.MAX_VALUE;

    for( final IElevationModel model : m_models )
      minElevation = Math.min( minElevation, model.getMinElevation() );

    return minElevation;
  }

  @Override
  public double getMaxElevation( ) throws ElevationException
  {
    double maxElevation = -Double.MAX_VALUE;

    for( final IElevationModel model : m_models )
      maxElevation = Math.min( maxElevation, model.getMinElevation() );

    return maxElevation;
  }
}