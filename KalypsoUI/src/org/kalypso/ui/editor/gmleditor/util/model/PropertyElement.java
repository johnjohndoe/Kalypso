/*
 * Created on Jan 19, 2005
 *  
 */
package org.kalypso.ui.editor.gmleditor.util.model;

import java.util.ArrayList;
import java.util.List;
import org.deegree.model.feature.FeatureAssociationTypeProperty;

/**
 * @author F.Lindemann
 *  
 */
public class PropertyElement extends Model
{

  protected List elements;

  private FeatureAssociationTypeProperty property = null;

  public PropertyElement( FeatureAssociationTypeProperty fatp )
  {
    elements = new ArrayList();
    this.name = fatp.getName().trim();
    property = fatp;
  }

  public Object[] getElements()
  {
    return elements.toArray();
  }

  public void addFeature( FeatureElement ft )
  {
    elements.add( ft );
    ft.parent = this;
  }

  public void addLinkedFeature( LinkedFeatureElement lfe )
  {
    elements.add( lfe );
    lfe.parent = this;
  }

  public FeatureAssociationTypeProperty getProperty()
  {
    return property;
  }

  public void setProperty( FeatureAssociationTypeProperty m_property )
  {
    this.property = m_property;
  }

  /**
   * @see org.kalypso.ui.editor.gmleditor.util.model.Model#remove(org.kalypso.ui.editor.gmleditor.util.model.Model)
   */
  public void remove( Model model )
  {    
    elements.remove(model);    
  }
}