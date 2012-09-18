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

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypsodeegree.model.elevation.ElevationException;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * Default {@link AbstractFeatureBinder} based implementation of {@link ITerrainElevationModelSystem}
 *
 * @author Patrice Congo
 * @author Madanagopal
 */
public class TerrainElevationModelSystem extends Feature_Impl implements ITerrainElevationModelSystem
{
  public static final QName SIM_BASE_F_TERRAIN_ELE_SYS = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "TerrainElevationModelSystem" ); //$NON-NLS-1$

  public static final QName SIM_BASE_PROP_TERRAIN_ELE_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "terrainElevationModel" ); //$NON-NLS-1$

  private final IFeatureBindingCollection<ITerrainElevationModel> m_terrainElevationModels = new FeatureBindingCollection<>( this, ITerrainElevationModel.class, SIM_BASE_PROP_TERRAIN_ELE_MODEL );

  public TerrainElevationModelSystem( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public void dispose( )
  {
    for( final ITerrainElevationModel elevationModel : m_terrainElevationModels )
      elevationModel.dispose();
  }

  @Override
  public IFeatureBindingCollection<ITerrainElevationModel> getTerrainElevationModels( )
  {
    return m_terrainElevationModels;
  }

  @Override
  public double getElevation( final GM_Point location )
  {
    for( final ITerrainElevationModel terrainElevationModel : m_terrainElevationModels )
    {
      try
      {
        final double elevation = terrainElevationModel.getElevation( location );
        if( !Double.isNaN( elevation ) )
        {
          return elevation;
        }
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
    return Double.NaN;
  }

  @Override
  public GM_Envelope getBoundingBox( )
  {
    GM_Envelope env = null;

    for( final ITerrainElevationModel tem : m_terrainElevationModels )
    {
      try
      {
        final GM_Envelope temEnv = tem.getBoundingBox();

        if( env == null )
          env = temEnv;
        else if( temEnv != null )
          env = env.getMerged( temEnv );
      }
      catch( final ElevationException e )
      {
        e.printStackTrace();
      }
    }

    return env;
  }

  @Override
  public double getMaxElevation( ) throws ElevationException
  {
    double maxEle = -Double.MAX_VALUE;
    for( final ITerrainElevationModel eleModel : m_terrainElevationModels )
    {
      final double curMaxEle = eleModel.getMaxElevation();

      maxEle = Math.max( maxEle, curMaxEle );
    }

    return maxEle == -Double.MAX_VALUE ? Double.NaN : maxEle;
  }

  @Override
  public double getMinElevation( ) throws ElevationException
  {
    double minEle = Double.MAX_VALUE;
    for( final ITerrainElevationModel eleModel : m_terrainElevationModels )
    {
      final double curMinEle = eleModel.getMinElevation();
      minEle = Math.min( minEle, curMinEle );
    }

    return minEle == Double.MAX_VALUE ? Double.NaN : minEle;
  }
}