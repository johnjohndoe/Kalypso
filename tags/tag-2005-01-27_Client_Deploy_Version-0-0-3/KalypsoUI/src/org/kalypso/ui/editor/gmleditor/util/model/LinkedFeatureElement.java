/*
 * Created on Jan 19, 2005
 *  
 */
package org.kalypso.ui.editor.gmleditor.util.model;

/**
 * @author F.Lindemann
 *  
 */
public class LinkedFeatureElement extends Model
{

  private FeatureElement linkedFeatureElement = null;

  public LinkedFeatureElement( String linkedFeatureName )
  {
    this.name = linkedFeatureName.trim();
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.ui.editor.gmleditor.util.model.Model#accept(org.kalypso.ui.editor.gmleditor.util.model.IModelVisitor,
   *      java.lang.Object)
   */
  public void accept( IModelVisitor visitor, Object passAlongArgument )
  {
  // TODO Auto-generated method stub

  }

  public FeatureElement getLinkedFeature()
  {
    return linkedFeatureElement;
  }

  public void setLinkedFeature( FeatureElement m_linkedFeatureElement )
  {
    this.linkedFeatureElement = m_linkedFeatureElement;
  }

  /**
   * @see org.kalypso.ui.editor.gmleditor.util.model.Model#remove(org.kalypso.ui.editor.gmleditor.util.model.Model)
   */
  public void remove( Model model )
  {/**/}
}