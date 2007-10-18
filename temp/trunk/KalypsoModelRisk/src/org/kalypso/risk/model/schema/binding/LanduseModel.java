package org.kalypso.risk.model.schema.binding;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

public class LanduseModel extends AbstractFeatureBinder implements ILanduseModel
{
  public LanduseModel( final Feature featureToBind )
  {
    super( featureToBind, ILanduseModel.QNAME );
  }

  public ILandusePolygonCollection getLandusePolygonCollection( )
  {
    final Feature feature = (Feature) getFeature().getProperty( ILanduseModel.PROPERTY_LANDUSE_COLLECTION );
    return (ILandusePolygonCollection) feature.getAdapter( ILandusePolygonCollection.class );
  }
}
