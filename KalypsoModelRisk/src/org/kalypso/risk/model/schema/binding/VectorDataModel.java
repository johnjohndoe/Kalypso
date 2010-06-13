package org.kalypso.risk.model.schema.binding;

import org.kalypso.afgui.model.UnversionedModel;
import org.kalypsodeegree.model.feature.Feature;

public class VectorDataModel extends UnversionedModel implements IVectorDataModel
{
  public VectorDataModel( final Feature featureToBind )
  {
    super( featureToBind, IVectorDataModel.QNAME );
  }

  @Override
  public ILandusePolygonCollection getLandusePolygonCollection( )
  {
    final Feature feature = (Feature) getFeature().getProperty( IVectorDataModel.PROPERTY_LANDUSE_COLLECTION );
    return (ILandusePolygonCollection) feature.getAdapter( ILandusePolygonCollection.class );
  }
}
