package org.kalypso.ogc.gml.featureview.control;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureTypeProperty;
import org.kalypso.ogc.gml.featureview.IFeatureControl;

/**
 * @author belger
 */
public abstract class AbstractFeatureControl implements IFeatureControl
{
  private Feature m_feature;

  private final FeatureTypeProperty m_ftp;
  
  public AbstractFeatureControl(  )
  {
    this( null, null );
  }

  public AbstractFeatureControl( final FeatureTypeProperty ftp )
  {
    this( null, ftp );
  }

  public AbstractFeatureControl( final Feature feature, final FeatureTypeProperty ftp )
  {
    m_feature = feature;
    m_ftp = ftp;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#dispose()
   */
  public abstract void dispose();

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#getFeature()
   */
  public final Feature getFeature()
  {
    return m_feature;
  }
  
  public final void setFeature( final Feature feature )
  {
    m_feature = feature;
  }
  
  public FeatureTypeProperty getFeatureTypeProperty()
  {
    return m_ftp;
  }
}
