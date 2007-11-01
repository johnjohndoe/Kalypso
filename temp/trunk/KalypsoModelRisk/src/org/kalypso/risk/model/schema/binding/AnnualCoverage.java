package org.kalypso.risk.model.schema.binding;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;

public class AnnualCoverage extends AbstractFeatureBinder implements IAnnualCoverage
{
  public AnnualCoverage( final Feature feature )
  {
    super( feature, IAnnualCoverage.QNAME );
  }

  public Integer getReturnPeriod( )
  {
    final Integer integer = (Integer) getFeature().getProperty( IAnnualCoverage.PROP_ANNUALITY );
    if( integer == null )
      return -1;
    return integer;
  }

  public void setReturnPeriod( final Integer value )
  {
    getFeature().setProperty( IAnnualCoverage.PROP_ANNUALITY, value == null ? "" : value.intValue() );
  }

  public GM_Envelope getEnvelope( )
  {
    return getFeature().getEnvelope();
  }

  public RectifiedGridCoverage getCoverage( )
  {
    final Feature feature = (Feature) getFeature().getProperty( IAnnualCoverage.PROP_COVERAGE );
    if( feature != null )
      return new RectifiedGridCoverage( feature );
    return null;
  }

  public void setCoverage( final RectifiedGridCoverage coverage )
  {
    getFeature().setProperty( IAnnualCoverage.PROP_COVERAGE, coverage.getWrappedFeature() );
  }
}
