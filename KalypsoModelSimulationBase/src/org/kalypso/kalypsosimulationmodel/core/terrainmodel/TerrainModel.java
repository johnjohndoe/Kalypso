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

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.VersionedModel;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * @author Gernot Belger
 */
public class TerrainModel extends VersionedModel implements ITerrainModel
{

  public static final QName SIM_BASE_PROP_TERRAIN_ELE_SYS = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "terrainElevationModelSystem" ); //$NON-NLS-1$

  private final IFeatureWrapperCollection<IRoughnessLayer> m_roughnessLayers = new FeatureWrapperCollection<IRoughnessLayer>( getFeature(), IRoughnessLayer.class, QNAME_PROP_ROUGHNESSLAYERPOLYGONCOLLECTION );

  public TerrainModel( final Feature featureToBind )
  {
    super( featureToBind, QNAME_TERRAIN_MODEL );

  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerainModel#getRiverProfileNetworkCollection()
   */
  @Override
  public IRiverProfileNetworkCollection getRiverProfileNetworkCollection( )
  {
    final Feature feature = (Feature) getFeature().getProperty( QNAME_PROP_RIVERPROFILENETWORKCOLLECTIONMEMBER );
    if( feature == null )
      return null;

    return (IRiverProfileNetworkCollection) feature.getAdapter( IRiverProfileNetworkCollection.class );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel#getRoughnessPolygonCollection()
   */
  @Override
  public List<IRoughnessPolygonCollection> getRoughnessPolygonCollections( )
  {
    final List<IRoughnessPolygonCollection> list = new ArrayList<IRoughnessPolygonCollection>();
    for( final IRoughnessLayer layer : m_roughnessLayers )
      list.add( getRoughnessPolygonCollection( layer ) );
    return list;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel#getRoughnessLayerCollection(org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessLayer)
   */
  @Override
  public IRoughnessPolygonCollection getRoughnessPolygonCollection( final IRoughnessLayer roughnessLayer )
  {
    return new RoughnessPolygonCollection( roughnessLayer.getFeature(), IRoughnessPolygon.class, QNAME_PROP_ROUGHNESSLAYERMEMBER );
  }

  @Override
  public ITerrainElevationModelSystem getTerrainElevationModelSystem( )
  {
    final Feature feature = (Feature) getFeature().getProperty( SIM_BASE_PROP_TERRAIN_ELE_SYS );

    if( feature == null )
    {
      return null;
    }
    else
    {
      return (ITerrainElevationModelSystem) feature.getAdapter( ITerrainElevationModelSystem.class );
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel#getRoughnessLayerCollection()
   */
  @Override
  public IFeatureWrapperCollection<IRoughnessLayer> getRoughnessLayerCollection( )
  {
    return m_roughnessLayers;
    // return new RoughnessLayerCollection( ((FeatureList) getFeature().getProperty(
    // ITerrainModel.QNAME_PROP_ROUGHNESSLAYERPOLYGONCOLLECTION )).getParentFeature(), IRoughnessLayer.class,
    // QNAME_PROP_ROUGHNESSLAYERPOLYGONCOLLECTION );
  }

}
