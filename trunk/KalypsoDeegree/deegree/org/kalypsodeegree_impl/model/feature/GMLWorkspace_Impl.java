package org.deegree_impl.model.feature;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureAssociationTypeProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.gml.schema.GMLSchema;

/**
 * @author doemming
 */
public class GMLWorkspace_Impl implements GMLWorkspace
{
  private final GMLSchema m_schema;
  private final Feature m_rootFeature;
  // final UndoManager ??

  /**
   *  HashMap(featuretype,List(feature))
  * TODO:	 HashMap(featuretype,HashMap(id,feature)) 
  */
  private final HashMap m_featureMap=new HashMap(); 
  
  public GMLWorkspace_Impl(GMLSchema schema,Feature feature)
  {
    m_schema=schema;
    FeatureType[] featureTypes = m_schema.getFeatureTypes();
    for(int i=0;i<featureTypes.length;i++)
    {
    	m_featureMap.put(featureTypes[i],new ArrayList());
    }
    m_rootFeature=feature;    
    registerFeature(feature);
  }
  
  private void addFeature(Feature feature)
  {
  	List list=(List)m_featureMap.get(feature.getFeatureType());
  	list.add(feature);
  }
  
  private void registerFeature(Feature feature) 
  {
  	if(feature==null)
  		return;
  	addFeature(feature);
	FeatureType featureType = feature.getFeatureType();
  	FeatureTypeProperty[] ftps = featureType.getProperties();
  	for(int i=0;i<ftps.length;i++)
  	{
  		if(ftps[i] instanceof FeatureAssociationTypeProperty)
  		{
  			Object value = feature.getProperty(ftps[i].getName());
  			if(value instanceof Feature)
  				registerFeature((Feature)value);
  			if(value instanceof List)
  				registerFeature((List)value);
  		}
  	}	
  }
  private void registerFeature(List list)
  {
  	if(list==null)
  		return;
  	for (Iterator iter = list.iterator(); iter.hasNext();) 
  		{
			Object value =  iter.next();
			if(value instanceof Feature)
				registerFeature((Feature)value);
		}
  }

public Feature getRootFeature()
  {
    return m_rootFeature;
  }

public GMLSchema getSchema()
  {
    return m_schema;
  }

  public FeatureType[] getFeatureTypes() 
  {
	return m_schema.getFeatureTypes(); 
  }

  public Feature[] getFeatures(FeatureType ft) 
  {
  	List list=(List)m_featureMap.get(ft);
  	return (Feature[]) list.toArray(new Feature[list.size()]);
  }
}
