package org.kalypso.risk.model.schema.binding;

import org.kalypso.afgui.model.UnversionedModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;

public class RasterDataModel extends UnversionedModel implements IRasterDataModel
{
  private final IFeatureBindingCollection<IAnnualCoverageCollection> m_waterlevelCoverageCollection;

  private final IFeatureBindingCollection<IAnnualCoverageCollection> m_specificDamageCoverageCollection;

  public RasterDataModel( final Feature featureToBind )
  {
    super( featureToBind, IRasterDataModel.QNAME );
    final Feature f1 = (Feature) getFeature().getProperty( IRasterDataModel.PROPERTY_WATERLEVEL_COVERAGE_COLLECTION );
    m_waterlevelCoverageCollection = new FeatureBindingCollection<IAnnualCoverageCollection>( f1, IAnnualCoverageCollection.class, IRasterDataModel.PROPERTY_ANNUAL_COVERAGE_MEMBER );
    final Feature f2 = (Feature) getFeature().getProperty( IRasterDataModel.PROPERTY_SPECIFIC_DAMAGE_COVERAGE_COLLECTION );
    m_specificDamageCoverageCollection = new FeatureBindingCollection<IAnnualCoverageCollection>( f2, IAnnualCoverageCollection.class, IRasterDataModel.PROPERTY_ANNUAL_COVERAGE_MEMBER );
  }

  @Override
  public ICoverageCollection getLanduseCoverage( )
  {
    final Feature feature = (Feature) getFeature().getProperty( IRasterDataModel.PROPERTY_LANDUSE_COVERAGE );
    if( feature != null )
      return (ICoverageCollection) feature.getAdapter( ICoverageCollection.class );
    return null;
  }

  @Override
  public ICoverageCollection getRiskZonesCoverage( )
  {
    final Feature feature = (Feature) getFeature().getProperty( IRasterDataModel.PROPERTY_RISK_ZONES_COVERAGE );
    if( feature != null )
      return (ICoverageCollection) feature.getAdapter( ICoverageCollection.class );
    return null;
  }

  @Override
  public IFeatureBindingCollection<IAnnualCoverageCollection> getSpecificDamageCoverageCollection( )
  {
    return m_specificDamageCoverageCollection;
  }

  // TODO: change name to Water-Depth! (everywhere...!)
  @Override
  public IFeatureBindingCollection<IAnnualCoverageCollection> getWaterlevelCoverageCollection( )
  {
    return m_waterlevelCoverageCollection;
  }
}