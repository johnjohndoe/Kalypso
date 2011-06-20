package org.kalypso.risk.model.schema.binding;

import org.kalypso.afgui.model.UnversionedModel;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;

public class VectorDataModel extends UnversionedModel implements IVectorDataModel
{
  
  public VectorDataModel( Object parent, IRelationType parentRelation, IFeatureType ft, String id, Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public ILandusePolygonCollection getLandusePolygonCollection( )
  {
    final Feature feature = (Feature) getProperty( IVectorDataModel.PROPERTY_LANDUSE_COLLECTION );
    return (ILandusePolygonCollection) feature.getAdapter( ILandusePolygonCollection.class );
  }
}
