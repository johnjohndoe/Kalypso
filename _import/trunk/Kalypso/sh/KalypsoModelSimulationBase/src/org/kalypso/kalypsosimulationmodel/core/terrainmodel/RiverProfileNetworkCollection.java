/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.util.List;

import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class RiverProfileNetworkCollection extends FeatureWrapperCollection<IRiverProfileNetwork> implements IRiverProfileNetworkCollection
{
  public RiverProfileNetworkCollection( final Feature feature )
  {
    super( feature, IRiverProfileNetwork.class, IRiverProfileNetworkCollection.QNAME_PROP_PROFILE_NETWORK );
  }

  public List<IRiverProfileNetwork> selectRiverProfileNetwork( final String regExp )
  {
    // TODO implement it
    return null;
  }

}
