package org.kalypso.ui.editor.gmleditor.util.model;

/**
 * @author F.Lindemann
 *  
 */
public class LinkedFeatureElement extends Model
{
  private FeatureElement linkedFeatureElement = null;

  public LinkedFeatureElement( final IModel parent, final String linkedFeatureName )
  {
    super( parent, linkedFeatureName.trim() );
  }

  public FeatureElement getLinkedFeature()
  {
    return linkedFeatureElement;
  }

  public void setLinkedFeature( FeatureElement m_linkedFeatureElement )
  {
    this.linkedFeatureElement = m_linkedFeatureElement;
  }
}