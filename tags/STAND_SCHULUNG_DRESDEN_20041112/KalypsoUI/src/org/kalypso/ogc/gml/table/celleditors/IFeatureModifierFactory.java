package org.kalypso.ogc.gml.table.celleditors;

import org.deegree.model.feature.FeatureTypeProperty;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;

/**
 * @author Belger
 */
public interface IFeatureModifierFactory
{
  public IFeatureModifier createFeatureModifier( final FeatureTypeProperty ftp );
}
