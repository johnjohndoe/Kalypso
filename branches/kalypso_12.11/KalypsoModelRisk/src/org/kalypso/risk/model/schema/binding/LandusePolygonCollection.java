package org.kalypso.risk.model.schema.binding;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

public class LandusePolygonCollection extends Feature_Impl implements ILandusePolygonCollection
{
  private final IFeatureBindingCollection<ILandusePolygon> m_landusePolygons = new FeatureBindingCollection<>( this, ILandusePolygon.class, ILandusePolygonCollection.PROPERTY_POLYGON_MEMBER );

  public LandusePolygonCollection( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public IFeatureBindingCollection<ILandusePolygon> getLandusePolygonCollection( )
  {
    return m_landusePolygons;
  }
}