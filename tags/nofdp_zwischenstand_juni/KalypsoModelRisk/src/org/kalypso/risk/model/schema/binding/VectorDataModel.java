package org.kalypso.risk.model.schema.binding;

import org.kalypso.kalypsosimulationmodel.core.UnversionedModel;
import org.kalypsodeegree.model.feature.Feature;

public class VectorDataModel extends UnversionedModel implements IVectorDataModel
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
