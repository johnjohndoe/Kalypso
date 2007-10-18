package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;

public class LanduseClassCollection extends FeatureWrapperCollection<ILanduseClass> implements ILanduseClassCollection
{
  public LanduseClassCollection( final Feature featureCol )
  {
    this(featureCol, ILanduseClass.class, ILanduseClassCollection.PROPERTY_LANDUSE_CLASS_MEMBER);
  }

  public LanduseClassCollection( final Feature featureCol, final Class<ILanduseClass> fwClass, final QName featureMemberProp )
  {
    super( featureCol, fwClass, featureMemberProp );
  }
}
