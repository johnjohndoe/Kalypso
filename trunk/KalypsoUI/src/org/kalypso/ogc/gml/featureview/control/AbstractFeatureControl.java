package org.kalypso.ogc.gml.featureview.control;

import org.deegree.model.feature.Feature;
import org.kalypso.ogc.gml.featureview.IFeatureControl;

/**
 * @author belger
 */
public abstract class AbstractFeatureControl implements IFeatureControl
{
  private final Feature m_feature;

  private final String m_propertyName;
  
  public AbstractFeatureControl( final Feature feature, final String propertyName )
  {
    m_feature = feature;
    m_propertyName = propertyName;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#dispose()
   */
  public void dispose()
  {
    // nix tun
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#getFeature()
   */
  public final Feature getFeature()
  {
    return m_feature;
  }
  
  public String getPropertyName()
  {
    return m_propertyName;
  }
  
  
}
