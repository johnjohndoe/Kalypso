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


import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * Default {@link AbstractFeatureBinder} based implementation
 * of {@link ITerrainElevationModelSystem} 
 * 
 * @author Patrice Congo
 * @author Madanagopal
 *
 */
public class TerrainElevationModelSystem 
                            extends AbstractFeatureBinder
                            implements ITerrainElevationModelSystem
{
  
  private final IFeatureWrapperCollection<ITerrainElevationModel> terrainElevationModels;
  
  /**
   * Creates a {@link TerrainElevationModelSystem} object binding the given feature.
   * the given feature must be substitutable to simBase:TerrainelevationModel
   * @throws IllegalArgumentException if featureToBind is null or not substitutable to
   *        simBase:TerrainElevationModel
   */
  public TerrainElevationModelSystem(
                        Feature featureToBind )
                        throws IllegalArgumentException
  {
    super( 
        featureToBind, 
        KalypsoModelSimulationBaseConsts.SIM_BASE_F_TERRAIN_ELE_SYS);
    terrainElevationModels= 
      new FeatureWrapperCollection<ITerrainElevationModel>(
            featureToBind, 
            ITerrainElevationModel.class, 
            KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_TERRAIN_ELE_MODEL);
    
  }
  
  
  public TerrainElevationModelSystem(
                          ITerrainModel terrainModel)
                          throws IllegalArgumentException
  {
    this(createTEMSysForTerrainModel(terrainModel));
  }

  /**
   * Creates or return a new feature for the give n terrian model
   */
  private static final Feature createTEMSysForTerrainModel(ITerrainModel terrainModel)
  {
    Assert.throwIAEOnNullParam( terrainModel, "terrainModel" );
     ITerrainElevationModelSystem temSys=terrainModel.getTerrainElevationModelSystem();
     if(temSys!=null)
     {
       return temSys.getWrappedFeature();
     }
     else
     {
      Feature parentFeature = terrainModel.getWrappedFeature();
      Feature newFeature=
        Util.createFeatureAsProperty( 
                          parentFeature, 
                          KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_TERRAIN_ELE_SYS, 
                          KalypsoModelSimulationBaseConsts.SIM_BASE_F_TERRAIN_ELE_SYS);
     
      return newFeature;
     }
  }
  
  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem#getTerrainElevationModels()
   */
  public IFeatureWrapperCollection<ITerrainElevationModel> getTerrainElevationModels( )
  {
    return terrainElevationModels;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel#getElevation(org.kalypsodeegree.model.geometry.GM_Point)
   */
  public double getElevation( GM_Point location )
  {
    for(ITerrainElevationModel terrainElevationModel:terrainElevationModels)
    {
      double elevation=terrainElevationModel.getElevation( location );
      if(elevation!=Double.NaN)
      {
       return elevation; 
      }
    }
    return Double.NaN;
  }
  
  
}
