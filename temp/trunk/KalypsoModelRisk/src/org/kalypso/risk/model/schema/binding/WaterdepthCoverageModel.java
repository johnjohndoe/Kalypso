package org.kalypso.risk.model.schema.binding;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

public class WaterdepthCoverageModel extends AbstractFeatureBinder implements IWaterdepthCoverageModel
{

  public WaterdepthCoverageModel( final Feature featureToBind )
  {
    super( featureToBind, IWaterdepthCoverageModel.QNAME );
  }

  public IFeatureWrapperCollection<IWaterdepthCoverage> getWaterdepthCoverageCollection( )
  {
    final Feature feature = (Feature) getFeature().getProperty( IWaterdepthCoverageModel.PROPERTY_WATERDEPTH_COVERAGE_COLLECTION );
    return (IWaterdepthCoverageCollection) feature.getAdapter( IWaterdepthCoverageCollection.class );
  }

}
