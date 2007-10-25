package org.kalypso.risk.model.schema.binding;

import ogc31.www.opengis.net.gml.RangeSetType;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;

public class WaterdepthCoverage extends AbstractFeatureBinder implements IWaterdepthCoverage
{
  public WaterdepthCoverage( final Feature feature )
  {
    super( feature, IWaterdepthCoverage.QNAME );
  }

  public Integer getReturnPeriod( )
  {
    return (Integer) getFeature().getProperty( IWaterdepthCoverage.PROP_RETURN_PERIOD );
  }

  public void setReturnPeriod( final Integer value )
  {
    getFeature().setProperty( IWaterdepthCoverage.PROP_RETURN_PERIOD, value == null ? "" : value.intValue() );
  }

  public RectifiedGridDomain getGridDomain( )
  {
    return (RectifiedGridDomain) getFeature().getProperty( IWaterdepthCoverage.PROP_RECTIFIED_GRID_DOMAIN );
  }

  public void setGridDomain( final RectifiedGridDomain gridDomain )
  {
    getFeature().setProperty( IWaterdepthCoverage.PROP_RECTIFIED_GRID_DOMAIN, gridDomain );
  }

  public RangeSetType getRangeSet( )
  {
    return (RangeSetType) getFeature().getProperty( IWaterdepthCoverage.PROP_RANGE_SET );
  }

  public void setRangeSet( final RangeSetType rangeSet )
  {
    getFeature().setProperty( IWaterdepthCoverage.PROP_RANGE_SET, rangeSet );
  }
}
