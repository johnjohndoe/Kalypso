package org.kalypso.risk.model.schema.binding;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

public class LanduseVectorModel extends AbstractFeatureBinder implements ILanduseVectorModel
{
  public LanduseVectorModel( final Feature featureToBind )
  {
    super( featureToBind, ILanduseVectorModel.QNAME );
  }

  public ILandusePolygonCollection getLandusePolygonCollection( )
  {
    final Feature feature = (Feature) getFeature().getProperty( ILanduseVectorModel.PROPERTY_LANDUSE_COLLECTION );
    return (ILandusePolygonCollection) feature.getAdapter( ILandusePolygonCollection.class );
  }
}
