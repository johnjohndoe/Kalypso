/*
 * Created on Jan 19, 2005
 *  
 */
package org.kalypso.ui.editor.gmleditor.util.model;

import java.util.ArrayList;
import java.util.List;

import org.deegree.model.feature.Feature;

/**
 * @author F.Lindemann
 *  
 */
public class FeatureElement extends Model
{

  protected List elements;

  private Feature feature = null;

  private FeatureElement()
  {
    elements = new ArrayList();    
  }

  public FeatureElement( Feature ft )
  {
    this();
    this.name = ft.getId().trim();
    feature = ft;
  }

  public static FeatureElement createRootFeatureElement( FeatureElement fe )
  {
    FeatureElement f = new FeatureElement();
    fe.parent = f;
    f.elements.add( fe );
    return f;
  }

  public Object[] getElements()
  {
    return elements.toArray();
  }

  public void addProperty( PropertyElement element )
  {
    elements.add( element );
    element.parent = this;
  }

  public Feature getFeature()
  {
    return feature;
  }

  public void setFeature( Feature m_feature )
  {
    this.feature = m_feature;
  }

  /**
   * @see org.kalypso.ui.editor.gmleditor.util.model.Model#remove(org.kalypso.ui.editor.gmleditor.util.model.Model)
   */
  public void remove( Model model )
  {
    elements.remove(model);    
  }
}