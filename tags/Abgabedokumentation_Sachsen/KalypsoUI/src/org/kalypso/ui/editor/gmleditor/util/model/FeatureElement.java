package org.kalypso.ui.editor.gmleditor.util.model;

import org.kalypsodeegree.model.feature.Feature;

/**
 * @author F.Lindemann
 *  
 */
public class FeatureElement extends Model
{
  private Feature m_feature = null;

  public static FeatureElement createRootFeature()
  {
    return new FeatureElement( null, null ); 
  }
  
  public FeatureElement( final IModel parent, final  Feature ft )
  {
    super( parent, ft == null ? null : ft.getId().trim() );
    
    m_feature = ft;
  }

  public Feature getFeature()
  {
    return m_feature;
  }

  public void setFeature( final Feature m_feature )
  {
    this.m_feature = m_feature;
  }
}