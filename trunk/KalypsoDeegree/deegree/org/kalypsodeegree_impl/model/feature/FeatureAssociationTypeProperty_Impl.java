package org.kalypsodeegree_impl.model.feature;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.w3c.dom.Node;

/**
 * @author doemming
 */
public class FeatureAssociationTypeProperty_Impl extends FeatureTypeProperty_Impl implements
    FeatureAssociationTypeProperty
{
  private final GMLSchema m_schema;

  private final List m_associationFeatureTypes = new ArrayList(); // including

  // substitutions

  private Node m_associatedNode;

  private FeatureType m_associatedFT; // without substituion

  public FeatureAssociationTypeProperty_Impl( String name, String namespace, String type, boolean nullable,
      FeatureType associationFeatureType, Map annotation )
  {
    this( name, namespace, type, nullable, null, associationFeatureType, annotation );
  }

  public FeatureAssociationTypeProperty_Impl( String name, String namespace, String type, boolean nullable,
      GMLSchema schema, FeatureType associationFeatureType, Map annotation )
  {
    super( name, namespace, type, nullable, annotation );
    m_associatedFT = associationFeatureType;
    // check if abstract
    m_associationFeatureTypes.add( associationFeatureType );
    m_schema = schema;
    m_associatedNode = null;
  }

  public FeatureAssociationTypeProperty_Impl( String name, String namespace, String type, boolean nullable,
      GMLSchema schema, Node node, Map annotation )
  {
    super( name, namespace, type, nullable, annotation );
    m_associatedFT = null;
    m_schema = schema;
    m_associatedNode = node;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty#getAssociationFeatureType()
   */
  public FeatureType[] getAssociationFeatureTypes()
  {
    checkAssociationBuildingStatus();
    return (FeatureType[])m_associationFeatureTypes.toArray( new FeatureType[m_associationFeatureTypes.size()] );
  }

  private void checkAssociationBuildingStatus()
  {
    if( m_associatedFT == null && m_associatedNode != null )
    {
      m_associatedFT = (FeatureType)m_schema.getMappedType( m_associatedNode );
      // TODO if m_associatedFT is abstract, do not add it
      m_associationFeatureTypes.add( m_associatedFT );
    }
  }

  /**
   * 
   * @see org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty#getAssociationFeatureType()
   */
  public FeatureType getAssociationFeatureType()
  {
    checkAssociationBuildingStatus();
    if( m_associatedFT == null )
      return m_associatedFT;
    return m_associatedFT;
  }

  public void registerSubstitution( FeatureType ft )
  {
    if( !m_associationFeatureTypes.contains( ft ) )
      m_associationFeatureTypes.add( ft );
  }

}