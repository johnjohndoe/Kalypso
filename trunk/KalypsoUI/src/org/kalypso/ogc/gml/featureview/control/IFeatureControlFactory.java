package org.kalypso.ogc.gml.featureview.control;

import org.deegree.model.feature.FeatureTypeProperty;
import org.kalypso.util.factory.FactoryException;

/**
 * @author belger
 */
public interface IFeatureControlFactory
{
  public boolean isDataTypeKnown( final FeatureTypeProperty ftp );
  
  public IFeatureControl createFeatureControl( final FeatureTypeProperty ftp ) throws FactoryException;
}
