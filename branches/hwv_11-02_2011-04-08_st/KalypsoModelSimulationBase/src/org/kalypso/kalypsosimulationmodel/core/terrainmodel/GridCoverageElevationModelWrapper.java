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
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import javax.xml.namespace.QName;

import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.grid.RectifiedGridCoverageGeoGrid;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

/**
 * @author Patrice Congo
 * @author Madanagopal
 * 
 */
public class GridCoverageElevationModelWrapper extends AbstractFeatureBinder implements ITerrainElevationModel, IFeatureWrapper2
{
  public static final QName SIM_BASE_PROP_GRID_COVERAGE = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "gridCoverage" ); //$NON-NLS-1$

  public static final QName SIM_BASE_F_GRID_COVERAGE_ELE_MODEL_WRAPPER = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "GridCoverageElevationModelWrapper" ); //$NON-NLS-1$

  private final IGeoGrid doubleRaster;

  public GridCoverageElevationModelWrapper( final Feature featureToBind ) throws Exception
  {
    this( featureToBind, SIM_BASE_F_GRID_COVERAGE_ELE_MODEL_WRAPPER );
  }

  public GridCoverageElevationModelWrapper( final Feature featureToBind, final QName qnameToBind ) throws Exception
  {
    super( featureToBind, qnameToBind );
    final Feature coverageFeature = (Feature) featureToBind.getProperty( SIM_BASE_PROP_GRID_COVERAGE );
    doubleRaster = new RectifiedGridCoverageGeoGrid( coverageFeature );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getElevation(org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public double getElevation( final GM_Point location )
  {
    try
    {
      final double xDouble = (location.getX() - doubleRaster.getOrigin().x);
      final double yDouble = (location.getY() - doubleRaster.getOrigin().y);

      final int x = (int) Math.floor( xDouble / doubleRaster.getSizeX() );
      final int y = (int) Math.floor( yDouble / doubleRaster.getSizeY() );

      if( x >= doubleRaster.getSizeX() || y >= doubleRaster.getSizeY() || x < 0 || y < 0 )
      {
        return Double.NaN;
      }

      return doubleRaster.getValue( x, y );
    }
    catch( final GeoGridException e )
    {
      e.printStackTrace();

      return Double.NaN;
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getBoundingBox()
   */
  @Override
  public GM_Envelope getBoundingBox( )
  {
    return null;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getCoordinateSystem()
   */
  @Override
  public String getCoordinateSystem( )
  {
    // TODO Patrice get crs from grid coverate
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getMaxElevation()
   */
  @Override
  public double getMaxElevation( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getMinElevation()
   */
  @Override
  public double getMinElevation( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#setCoordinateSystem(java.lang.String)
   */
  @Override
  public void setCoordinateSystem( final String coordinateSystem )
  {
    throw new UnsupportedOperationException();
  }
}
