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
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import javax.xml.namespace.QName;

import org.kalypso.afgui.model.Util;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Default {@link AbstractFeatureBinder} based implementation of {@link ITerrainElevationModelSystem}
 * 
 * @author Patrice Congo
 * @author Madanagopal
 */
public class TerrainElevationModelSystem extends AbstractFeatureBinder implements ITerrainElevationModelSystem
{
  public static final QName SIM_BASE_F_TERRAIN_ELE_SYS = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "TerrainElevationModelSystem" ); //$NON-NLS-1$

  public static final QName SIM_BASE_PROP_TERRAIN_ELE_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "terrainElevationModel" ); //$NON-NLS-1$

  private final IFeatureWrapperCollection<ITerrainElevationModel> terrainElevationModels;

  /**
   * Creates a {@link TerrainElevationModelSystem} object binding the given feature. the given feature must be
   * substitutable to simBase:TerrainelevationModel
   * 
   * @throws IllegalArgumentException
   *           if featureToBind is null or not substitutable to simBase:TerrainElevationModel
   */
  public TerrainElevationModelSystem( final Feature featureToBind ) throws IllegalArgumentException
  {
    super( featureToBind, SIM_BASE_F_TERRAIN_ELE_SYS );
    terrainElevationModels = new FeatureWrapperCollection<ITerrainElevationModel>( featureToBind, ITerrainElevationModel.class, SIM_BASE_PROP_TERRAIN_ELE_MODEL );

  }

  public TerrainElevationModelSystem( final ITerrainModel terrainModel ) throws IllegalArgumentException
  {
    this( createTEMSysForTerrainModel( terrainModel ) );
  }

  /**
   * Creates or return a new feature for the give n terrian model
   */
  private static final Feature createTEMSysForTerrainModel( final ITerrainModel terrainModel )
  {
    Assert.throwIAEOnNullParam( terrainModel, "terrainModel" ); //$NON-NLS-1$
    final ITerrainElevationModelSystem temSys = terrainModel.getTerrainElevationModelSystem();
    if( temSys != null )
    {
      return temSys.getFeature();
    }
    else
    {
      final Feature parentFeature = terrainModel.getFeature();
      final Feature newFeature = Util.createFeatureAsProperty( parentFeature, TerrainModel.SIM_BASE_PROP_TERRAIN_ELE_SYS, SIM_BASE_F_TERRAIN_ELE_SYS );

      return newFeature;
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem#getTerrainElevationModels()
   */
  @Override
  public IFeatureWrapperCollection<ITerrainElevationModel> getTerrainElevationModels( )
  {
    return terrainElevationModels;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel#getElevation(org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public double getElevation( final GM_Point location )
  {
    for( final ITerrainElevationModel terrainElevationModel : terrainElevationModels )
    {
      try
      {
        final double elevation = terrainElevationModel.getElevation( location );
        if( elevation != Double.NaN )
        {
          return elevation;
        }
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
    return Double.NaN;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getBoundingBox()
   */
  @Override
  public GM_Envelope getBoundingBox( )
  {
    GM_Envelope env = null;
    int i = terrainElevationModels.size() - 1;

    // find the first non null envelop and init the merged env
    firstNonNullEnv: for( ; i >= 0; i-- )
    {
      final GM_Envelope boundingBox = terrainElevationModels.get( i ).getBoundingBox();
      if( boundingBox != null )
      {
        final GM_Position min = boundingBox.getMin();
        final GM_Position max = boundingBox.getMax();
        env = GeometryFactory.createGM_Envelope( min.getX(), min.getY(), max.getX(), max.getY(), boundingBox.getCoordinateSystem() );
        break firstNonNullEnv;
      }
    }

    // merge other env
    for( ; i >= 0; i-- )
    {
      final GM_Envelope boundingBox = terrainElevationModels.get( i ).getBoundingBox();
      if( boundingBox != null )
      {
        env = env.getMerged( boundingBox );
      }
    }

    return env;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getCoordinateSystem()
   */
  @Override
  public String getCoordinateSystem( )
  {
    // TODO Patrice check whether the elevation do have the same system and return it
    return null;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getMaxElevation()
   */
  @Override
  public double getMaxElevation( )
  {
    if( terrainElevationModels.isEmpty() )
    {
      return Double.NaN;
    }

    double maxEle = -Double.MAX_VALUE;
    double curMaxEle;
    for( final ITerrainElevationModel eleModel : terrainElevationModels )
    {
      curMaxEle = eleModel.getMaxElevation();
      if( maxEle < curMaxEle )
      {
        maxEle = curMaxEle;
      }
    }

    return maxEle == -Double.MAX_VALUE ? Double.NaN : maxEle;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getMinElevation()
   */
  @Override
  public double getMinElevation( )
  {
    if( terrainElevationModels.isEmpty() )
    {
      return Double.NaN;
    }

    double minEle = Double.MAX_VALUE;
    double curMinEle;
    for( final ITerrainElevationModel eleModel : terrainElevationModels )
    {
      curMinEle = eleModel.getMinElevation();
      if( minEle > curMinEle )
      {
        minEle = curMinEle;
      }
    }

    return minEle == Double.MAX_VALUE ? Double.NaN : minEle;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#setCoordinateSystem(java.lang.String)
   */
  @Override
  public void setCoordinateSystem( final String coordinateSystem )
  {
    // TODO

  }
}
