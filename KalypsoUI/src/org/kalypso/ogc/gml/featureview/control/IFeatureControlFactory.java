package org.kalypso.ogc.gml.featureview.control;

import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.util.factory.FactoryException;

/**
 * @author belger
 */
public interface IFeatureControlFactory
{
  public boolean isDataTypeKnown( final FeatureTypeProperty ftp );
  
  public AbstractFeatureControl createFeatureControl( final Composite parent, final int style, final FeatureTypeProperty ftp ) throws FactoryException;
}
