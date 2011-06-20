/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.util.List;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Gernot Belger
 */
public class RiverProfileNetworkCollection extends Feature_Impl implements IRiverProfileNetworkCollection
{
  private final IFeatureBindingCollection<IRiverProfileNetwork> m_riverProfileNetworks = new FeatureBindingCollection<IRiverProfileNetwork>( this, IRiverProfileNetwork.class, IRiverProfileNetworkCollection.QNAME_PROP_PROFILE_NETWORK );

  public RiverProfileNetworkCollection( Object parent, IRelationType parentRelation, IFeatureType ft, String id, Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }
  
  public IFeatureBindingCollection<IRiverProfileNetwork> getRiverProfileNetworks( )
  {
    return m_riverProfileNetworks;
  }

  @Override
  public List<IRiverProfileNetwork> selectRiverProfileNetwork( final String regExp )
  {
    // TODO implement it
    return null;
  }

}
