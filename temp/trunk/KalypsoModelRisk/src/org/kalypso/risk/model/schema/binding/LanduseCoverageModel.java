package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

public class LanduseCoverageModel extends AbstractFeatureBinder implements ILanduseCoverageModel
{
  public LanduseCoverageModel( final Feature featureToBind )
  {
    this( featureToBind, ILanduseCoverageModel.QNAME );
  }

  public LanduseCoverageModel( final Feature featureToBind, final QName qnameToBind )
  {
    super( featureToBind, qnameToBind );
  }

  public ICoverageCollection getCoverageCollection( )
  {
    final Feature feature = (Feature) getFeature().getProperty( ILanduseCoverageModel.PROPERTY_COVERAGE_COLLECTION );
    return (ICoverageCollection) feature.getAdapter( ICoverageCollection.class );
  }
}
