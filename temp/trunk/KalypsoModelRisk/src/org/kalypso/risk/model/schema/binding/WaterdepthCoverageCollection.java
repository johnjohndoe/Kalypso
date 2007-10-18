package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

public class WaterdepthCoverageCollection extends FeatureWrapperCollection<IWaterdepthCoverage> implements IWaterdepthCoverageCollection
{
  private final IFeatureWrapperCollection<IWaterdepthCoverage> m_coverages = new FeatureWrapperCollection<IWaterdepthCoverage>( getFeature(), IWaterdepthCoverage.class, IWaterdepthCoverageCollection.PROPERTY_WATERDEPTH_COVERAGE_MEMBER );

  public WaterdepthCoverageCollection( final Feature featureCol )
  {
    this( featureCol, IWaterdepthCoverage.class, IWaterdepthCoverageCollection.PROPERTY_WATERDEPTH_COVERAGE_MEMBER );
  }

  public WaterdepthCoverageCollection( final Feature featureCol, final Class<IWaterdepthCoverage> fwClass, final QName featureMemberProp )
  {
    super( featureCol, fwClass, featureMemberProp );
  }

  public IFeatureWrapperCollection<IWaterdepthCoverage> getWaterdepthCoverageCollection( )
  {
    return m_coverages;
  }

}
