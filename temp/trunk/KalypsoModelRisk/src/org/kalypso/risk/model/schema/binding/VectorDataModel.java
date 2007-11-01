package org.kalypso.risk.model.schema.binding;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

public class VectorDataModel extends AbstractFeatureBinder implements IVectorDataModel
{
  public VectorDataModel( final Feature featureToBind )
  {
    super( featureToBind, IVectorDataModel.QNAME );
  }

  public ILandusePolygonCollection getLandusePolygonCollection( )
  {
    final Feature feature = (Feature) getFeature().getProperty( IVectorDataModel.PROPERTY_LANDUSE_COLLECTION );
    return (ILandusePolygonCollection) feature.getAdapter( ILandusePolygonCollection.class );
  }
}
