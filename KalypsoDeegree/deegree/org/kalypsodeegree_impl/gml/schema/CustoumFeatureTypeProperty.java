package org.kalypsodeegree_impl.gml.schema;

import java.util.Map;

import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree_impl.model.feature.FeatureTypeProperty_Impl;

/**
 * @author doemming
 * 
 * class acts as a marker for custoumproperties
 *  
 */
public class CustoumFeatureTypeProperty extends FeatureTypeProperty_Impl implements FeatureTypeProperty
{
  /** Andreas: ist es absicht, das dies die default visibility hat? Wenn ja, Comment? */
  CustoumFeatureTypeProperty( String name, String namespace, String type, boolean nullable, Map annotation )
  {
    super( name, namespace, type, nullable, annotation );
  }

}
