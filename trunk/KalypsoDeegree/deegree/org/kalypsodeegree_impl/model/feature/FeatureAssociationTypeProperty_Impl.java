package org.deegree_impl.model.feature;

import org.deegree.model.feature.FeatureAssociationTypeProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree_impl.gml.schema.GMLSchema;
import org.w3c.dom.Node;

/**
 * @author doemming
 */
public class FeatureAssociationTypeProperty_Impl extends FeatureTypeProperty_Impl implements
    FeatureAssociationTypeProperty
{
  private final GMLSchema m_schema;
  private FeatureType m_associationFeatureType=null;
  private final Node m_associatedNode;
  public FeatureAssociationTypeProperty_Impl( String name, String namespace, String type, boolean nullable,FeatureType associationFeatureType )
  {
    super( name, namespace, type, nullable ); 
    m_associationFeatureType=associationFeatureType;
    m_schema=null;
    m_associatedNode=null;
  }

  public FeatureAssociationTypeProperty_Impl( String name, String namespace, String type, boolean nullable, GMLSchema schema,Node node )
  {
    super( name, namespace, type, nullable ); 
    m_associationFeatureType=null;
    m_schema=schema;
    m_associatedNode=node;
  }

  /**
   * @see org.deegree.model.feature.FeatureAssociationTypeProperty#getAssociationFeatureType()
   */
  public FeatureType getAssociationFeatureType()
  {
    if(m_associationFeatureType==null)
    {
      m_associationFeatureType=(FeatureType)m_schema.getMappedType(m_associatedNode);
    //    m_schema.getFeatureType(getName());
    }
    return m_associationFeatureType;
  }

}
