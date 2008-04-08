package org.kalypso.risk.model.schema.binding;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.CoverageCollection;

/**
 * @author Dejan Antanskovic
 */
public class AnnualCoverage extends CoverageCollection implements IAnnualCoverageCollection
{
  public AnnualCoverage( final Feature feature )
  {
    super( feature );
  }

  public Integer getReturnPeriod( )
  {
    final Integer integer = (Integer) getFeature().getProperty( IAnnualCoverageCollection.PROP_ANNUALITY );
    if( integer == null )
      return -1;
    return integer;
  }

  public void setReturnPeriod( final Integer value )
  {
    getFeature().setProperty( IAnnualCoverageCollection.PROP_ANNUALITY, value == null ? "" : value.intValue() ); //$NON-NLS-1$
  }

}
