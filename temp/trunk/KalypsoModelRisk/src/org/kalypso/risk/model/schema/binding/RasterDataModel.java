package org.kalypso.risk.model.schema.binding;

import org.kalypso.afgui.model.UnversionedModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

public class RasterDataModel extends UnversionedModel implements IRasterDataModel
{
  private final IFeatureWrapperCollection<IAnnualCoverageCollection> m_waterlevelCoverageCollection;

  private final IFeatureWrapperCollection<IAnnualCoverageCollection> m_specificDamageCoverageCollection;

  public RasterDataModel( final Feature featureToBind )
  {
    super( featureToBind, IRasterDataModel.QNAME );
    final Feature f1 = (Feature) getFeature().getProperty( IRasterDataModel.PROPERTY_WATERLEVEL_COVERAGE_COLLECTION );
    m_waterlevelCoverageCollection = new FeatureWrapperCollection<IAnnualCoverageCollection>( f1, IAnnualCoverageCollection.class, IRasterDataModel.PROPERTY_ANNUAL_COVERAGE_MEMBER );
    final Feature f2 = (Feature) getFeature().getProperty( IRasterDataModel.PROPERTY_SPECIFIC_DAMAGE_COVERAGE_COLLECTION );
    m_specificDamageCoverageCollection = new FeatureWrapperCollection<IAnnualCoverageCollection>( f2, IAnnualCoverageCollection.class, IRasterDataModel.PROPERTY_ANNUAL_COVERAGE_MEMBER );
  }

  public ICoverageCollection getLanduseCoverage( )
  {
    final Feature feature = (Feature) getFeature().getProperty( IRasterDataModel.PROPERTY_LANDUSE_COVERAGE );
    if( feature != null )
      return (ICoverageCollection) feature.getAdapter( ICoverageCollection.class );
    return null;
  }

  public ICoverageCollection getRiskZonesCoverage( )
  {
    final Feature feature = (Feature) getFeature().getProperty( IRasterDataModel.PROPERTY_RISK_ZONES_COVERAGE );
    if( feature != null )
      return (ICoverageCollection) feature.getAdapter( ICoverageCollection.class );
    return null;
  }

  public IFeatureWrapperCollection<IAnnualCoverageCollection> getSpecificDamageCoverageCollection( )
  {
    return m_specificDamageCoverageCollection;
  }

  // TODO: change name to Water-Depth! (everywhere...!)
  public IFeatureWrapperCollection<IAnnualCoverageCollection> getWaterlevelCoverageCollection( )
  {
    return m_waterlevelCoverageCollection;
  }

}
