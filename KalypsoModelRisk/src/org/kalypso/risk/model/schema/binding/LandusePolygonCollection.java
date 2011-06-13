package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

public class LandusePolygonCollection extends FeatureWrapperCollection<ILandusePolygon> implements ILandusePolygonCollection
{
  private final IFeatureWrapperCollection<ILandusePolygon> m_landusePolygons = new FeatureWrapperCollection<ILandusePolygon>( getFeature(), ILandusePolygon.class, ILandusePolygonCollection.PROPERTY_POLYGON_MEMBER );

  public LandusePolygonCollection( final Feature featureCol )
  {
    this( featureCol, ILandusePolygon.class, ILandusePolygonCollection.PROPERTY_POLYGON_MEMBER );
  }

  public LandusePolygonCollection( final Feature featureCol, final Class<ILandusePolygon> fwClass, final QName featureMemberProp )
  {
    super( featureCol, fwClass, featureMemberProp );
  }

  @Override
  public IFeatureWrapperCollection<ILandusePolygon> getLandusePolygonCollection( )
  {
    return m_landusePolygons;
  }
}
