package org.kalypso.ui.editor.gmleditor.util.model;

import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;

/**
 * @author F.Lindemann
 *  
 */
public class PropertyElement extends Model
{
  private FeatureAssociationTypeProperty m_property = null;

  public PropertyElement( final IModel parent, final FeatureAssociationTypeProperty fatp )
  {
    super( parent, fatp.getName().trim() );

    m_property = fatp;
  }

//  public void addFeature( FeatureElement ft )
//  {
//    elements.add( ft );
//    ft.parent = this;
//  }
//
  public FeatureAssociationTypeProperty getProperty()
  {
    return m_property;
  }

  public void setProperty( final FeatureAssociationTypeProperty m_property )
  {
    this.m_property = m_property;
  }
}