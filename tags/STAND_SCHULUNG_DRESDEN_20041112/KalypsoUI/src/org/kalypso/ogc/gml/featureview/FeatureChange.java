package org.kalypso.ogc.gml.featureview;

import org.deegree.model.feature.Feature;

/**
 * @author belger
 */
public class FeatureChange
{
  public final Feature feature;
  public final String property;
  public final Object newValue;

  public FeatureChange( final Feature feature, final String property, final Object newValue )
  {
    this.feature = feature;
    this.property = property;
    this.newValue = newValue;
  }
}
