package org.kalypso.risk.model.schema.binding;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;

public class RasterDataModel extends AbstractFeatureBinder implements IRasterDataModel
{
  private final IFeatureWrapperCollection<IAnnualCoverage> m_waterlevelCoverageCollection;

  private final IFeatureWrapperCollection<IAnnualCoverage> m_specificDamageCoverageCollection;

  public RasterDataModel( final Feature featureToBind )
  {
    super( featureToBind, IRasterDataModel.QNAME );
    final Feature f1 = (Feature) getFeature().getProperty( IRasterDataModel.PROPERTY_WATERLEVEL_COVERAGE_COLLECTION );
    m_waterlevelCoverageCollection = new FeatureWrapperCollection<IAnnualCoverage>( f1, IAnnualCoverage.class, IRasterDataModel.PROPERTY_ANNUAL_COVERAGE_MEMBER );
    final Feature f2 = (Feature) getFeature().getProperty( IRasterDataModel.PROPERTY_SPECIFIC_DAMAGE_COVERAGE_COLLECTION );
    m_specificDamageCoverageCollection = new FeatureWrapperCollection<IAnnualCoverage>( f2, IAnnualCoverage.class, IRasterDataModel.PROPERTY_ANNUAL_COVERAGE_MEMBER );
  }

  public RectifiedGridCoverage getLanduseCoverage( )
  {
    final Feature feature = (Feature) getFeature().getProperty( IRasterDataModel.PROPERTY_LANDUSE_COVERAGE );
    if( feature != null )
      return new RectifiedGridCoverage( feature );
    return null;
  }

  public RectifiedGridCoverage getRiskZonesCoverage( )
  {
    final Feature feature = (Feature) getFeature().getProperty( IRasterDataModel.PROPERTY_RISK_ZONES_COVERAGE );
    if( feature != null )
      return new RectifiedGridCoverage( feature );
    return null;
  }

  public IFeatureWrapperCollection<IAnnualCoverage> getSpecificDamageCoverageCollection( )
  {
    return m_specificDamageCoverageCollection;
  }

  public IFeatureWrapperCollection<IAnnualCoverage> getWaterlevelCoverageCollection( )
  {
    return m_waterlevelCoverageCollection;
  }

}
