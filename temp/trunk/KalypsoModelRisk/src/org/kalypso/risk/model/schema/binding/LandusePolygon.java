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
    getFeature().setProperty( ILandusePolygon.PROPERTY_GEOMETRY, surface );
  }

  public void setStyleType( String styleType )
  {
    getFeature().setProperty( ILandusePolygon.PROPERTY_SLDSTYLE, styleType );
  }

  public String getStyleType( )
  {
    final Object value = getFeature().getProperty( ILandusePolygon.PROPERTY_SLDSTYLE );
    return (value != null && value != "") ? value.toString() : "_DEFAULT_STYLE_";
  }

  public void setLanduseClass( final Feature landuseClassFeature )
  {
    getFeature().setProperty( ILandusePolygon.PROPERTY_LANDUSE_CLASS, landuseClassFeature );
  }

}
