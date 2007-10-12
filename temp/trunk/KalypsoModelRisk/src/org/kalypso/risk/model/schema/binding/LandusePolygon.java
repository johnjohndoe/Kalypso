package org.kalypso.risk.model.schema.binding;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

public class LandusePolygon extends AbstractFeatureBinder implements ILandusePolygon
{

  public LandusePolygon( final Feature featureToBind )
  {
    super( featureToBind, QNAME );
  }

  public void setGeometry( GM_Surface< ? > surface )
  {
    getFeature().setProperty( ILandusePolygon.QNAME_PROPERTY_GEOMETRY, surface );
  }

  public void setStyleType( String styleType )
  {
    getFeature().setProperty( ILandusePolygon.QNAME_PROPERTY_SLDSTYLE, styleType );
  }

}
