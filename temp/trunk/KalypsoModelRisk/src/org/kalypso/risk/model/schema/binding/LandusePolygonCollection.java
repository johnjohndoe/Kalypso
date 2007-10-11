package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;

public class LandusePolygonCollection extends FeatureWrapperCollection<ILandusePolygon> implements ILandusePolygonCollection
{
  public LandusePolygonCollection( final Feature featureCol )
  {
    this( featureCol, ILandusePolygon.class, ILandusePolygonCollection.QNAME_PROPERTY_POLYGON );
  }

  public LandusePolygonCollection( final Feature featureCol, final Class<ILandusePolygon> fwClass, final QName featureMemberProp )
  {
    super( featureCol, fwClass, featureMemberProp );
  }
}
