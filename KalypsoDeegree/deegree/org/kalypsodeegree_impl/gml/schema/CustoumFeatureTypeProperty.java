/*
 */
package org.deegree_impl.gml.schema;

import java.util.Map;

import org.deegree_impl.model.feature.FeatureTypeProperty_Impl;


/**
 * @author doemming
 * 
 * class acts as a marker for custoumproperties
 * 
 */
public class CustoumFeatureTypeProperty extends FeatureTypeProperty_Impl 
{

	/**
	 * 
	 */
	CustoumFeatureTypeProperty(String name, String namespace, String type, boolean nullable,Map annotation) 
	{
		super(name, namespace, type, nullable,annotation);
	}

}
