package org.kalypsodeegree_impl.gml.schema.vistors;

import java.util.Arrays;
import java.util.HashSet;

import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;

/**
 * 
 * 
 * visitor that collectes the featuretypes used by schemas
 * 
 * @author thuel
 */
public class CollectFeatureTypesSchemaVisitor implements GMLSchemaVisitor
{

  private final HashSet m_featureTypes;

  public CollectFeatureTypesSchemaVisitor()
  {
    m_featureTypes = new HashSet();
  }

  public CollectFeatureTypesSchemaVisitor( final HashSet resultList )
  {
    if( resultList == null )
      m_featureTypes = new HashSet();
    else
      m_featureTypes = resultList;
  }

  public boolean visit( final GMLSchema schema )
  {
    m_featureTypes.addAll( Arrays.asList( schema.getFeatureTypes() ) );
    return true;
  }

  /**
   * 
   * @return featuretypes that are collected by the visitor
   */
  public FeatureType[] getFeatureTypes()
  {
    return (FeatureType[])m_featureTypes.toArray( new FeatureType[m_featureTypes.size()] );
  }
}