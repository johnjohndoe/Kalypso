package org.deegree_impl.model.feature;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

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

  private FeatureType[] m_associationFeatureTypes = null; // including

  // substitutions

  private final Node m_associatedNode;

  private final FeatureType m_associatedFT; // without substituion

  public FeatureAssociationTypeProperty_Impl( String name, String namespace, String type,
      boolean nullable, FeatureType associationFeatureType, Map annotation )
  {
    this( name, namespace, type, nullable, null, associationFeatureType, annotation );
  }
  
  public FeatureAssociationTypeProperty_Impl( String name, String namespace, String type,
      boolean nullable, GMLSchema schema, FeatureType associationFeatureType ,Map annotation)
  {
    super( name, namespace, type, nullable,annotation );
    m_associatedFT = associationFeatureType;
    m_associationFeatureTypes = null;//associationFeatureTypes;
    m_schema = schema;
    m_associatedNode = null;    
  }

  public FeatureAssociationTypeProperty_Impl( String name, String namespace, String type,
      boolean nullable, GMLSchema schema, Node node,Map annotation )
  {
    super( name, namespace, type, nullable,annotation );
    m_associatedFT = null;
    m_associationFeatureTypes = null;
    m_schema = schema;
    m_associatedNode = node;
  }

  /**
   * @see org.deegree.model.feature.FeatureAssociationTypeProperty#getAssociationFeatureType()
   */
  public FeatureType[] getAssociationFeatureTypes()
  {
    if( m_associationFeatureTypes == null )
    {
      FeatureType associationFeatureType = null;
      if( m_associatedNode != null )
        associationFeatureType = (FeatureType)m_schema.getMappedType( m_associatedNode );
      else if( m_associatedFT != null )
        associationFeatureType = m_associatedFT;
      List list = new ArrayList();

      if(m_schema!=null) // if we have a schema (shape files have none)
      {
      	FeatureType[] subStitutionFE = GMLHelper.getResolveSubstitutionGroup( associationFeatureType, m_schema.getFeatureTypes() );
        list.addAll( Arrays.asList( subStitutionFE ) );
        // TODO test if some featuretypes are abstract and do not add them
      }
      list.add( associationFeatureType );
      m_associationFeatureTypes = (FeatureType[])list.toArray( new FeatureType[list.size()] );

    }
    return m_associationFeatureTypes;
  }

  /**
   * @see org.deegree.model.feature.FeatureAssociationTypeProperty#getAcsociationFeatureType()
   */
  public FeatureType getAssociationFeatureType()
  {
    return m_associatedFT;
  }

}