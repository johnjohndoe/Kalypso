package org.kalypso.risk.model.schema.binding;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

public class RasterizationControlModel extends AbstractFeatureBinder implements IRasterizationControlModel
{
  public RasterizationControlModel( final Feature featureToBind )
  {
    super( featureToBind, IRasterizationControlModel.QNAME );
  }

  public ILanduseClassCollection getLanduseClassCollection( )
  {
    final Feature feature = (Feature) getFeature().getProperty( IRasterizationControlModel.PROPERTY_LANDUSE_CLASS_COLLECTION );
    return (ILanduseClassCollection) feature.getAdapter( ILanduseClassCollection.class );
  }
}
