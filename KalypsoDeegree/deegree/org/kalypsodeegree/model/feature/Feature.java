package org.deegree.model.feature;


/**
 * @author doemming
 * 
 * this class extends the deegree feature interface and implements
 * methods to handle properties that have maxOccurs > 1
 */
public interface Feature extends DeegreeFeature
{
  public void addProperty( FeatureProperty prop );
  
}