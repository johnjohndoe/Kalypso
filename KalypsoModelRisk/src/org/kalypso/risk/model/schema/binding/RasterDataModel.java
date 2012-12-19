package org.kalypso.risk.model.schema.binding;

import org.kalypso.afgui.model.UnversionedModel;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;

public class RasterDataModel extends UnversionedModel implements IRasterDataModel
{
  public RasterDataModel( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  private IFeatureBindingCollection<IAnnualCoverageCollection> m_waterlevelCoverageCollection;

  private IFeatureBindingCollection<IAnnualCoverageCollection> m_specificDamageCoverageCollection;

  @Override
  public ICoverageCollection getLanduseCoverage( )
  {
    final Feature feature = (Feature) getProperty( IRasterDataModel.PROPERTY_LANDUSE_COVERAGE );
    if( feature != null )
      return (ICoverageCollection) feature.getAdapter( ICoverageCollection.class );
    return null;
  }

  @Override
  public ICoverageCollection getRiskZonesCoverage( )
  {
    final Feature feature = (Feature) getProperty( IRasterDataModel.PROPERTY_RISK_ZONES_COVERAGE );
    if( feature != null )
      return (ICoverageCollection) feature.getAdapter( ICoverageCollection.class );
    return null;
  }

  @Override
  public IFeatureBindingCollection<IAnnualCoverageCollection> getSpecificDamageCoverageCollection( )
  {
    if( m_specificDamageCoverageCollection == null )
    {
      final Feature f2 = (Feature) getProperty( IRasterDataModel.PROPERTY_SPECIFIC_DAMAGE_COVERAGE_COLLECTION );
      m_specificDamageCoverageCollection = new FeatureBindingCollection<>( f2, IAnnualCoverageCollection.class, IRasterDataModel.PROPERTY_ANNUAL_COVERAGE_MEMBER );
    }
    return m_specificDamageCoverageCollection;
  }

  // TODO: change name to Water-Depth! (everywhere...!)
  @Override
  public IFeatureBindingCollection<IAnnualCoverageCollection> getWaterlevelCoverageCollection( )
  {
    if( m_waterlevelCoverageCollection == null )
    {
      final Feature f1 = (Feature) getProperty( IRasterDataModel.PROPERTY_WATERLEVEL_COVERAGE_COLLECTION );
      m_waterlevelCoverageCollection = new FeatureBindingCollection<>( f1, IAnnualCoverageCollection.class, IRasterDataModel.PROPERTY_ANNUAL_COVERAGE_MEMBER );
    }
    return m_waterlevelCoverageCollection;
  }
}