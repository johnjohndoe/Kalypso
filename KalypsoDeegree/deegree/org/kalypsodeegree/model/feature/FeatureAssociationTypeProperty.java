package org.kalypsodeegree.model.feature;

/**
 * @author doemming
 */
public interface FeatureAssociationTypeProperty extends FeatureTypeProperty
{
  public FeatureType getAssociationFeatureType();

  public FeatureType[] getAssociationFeatureTypes();

  public void registerSubstitution( FeatureType ftp );
}
