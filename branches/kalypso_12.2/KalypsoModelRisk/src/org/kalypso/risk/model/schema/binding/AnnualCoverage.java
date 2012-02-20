package org.kalypso.risk.model.schema.binding;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree_impl.gml.binding.commons.CoverageCollection;

/**
 * @author Dejan Antanaskovic
 */
public class AnnualCoverage extends CoverageCollection implements IAnnualCoverageCollection
{
  public AnnualCoverage( Object parent, IRelationType parentRelation, IFeatureType ft, String id, Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  /**
   * Gets the return period; if not set, the default value of 1 is returned (support for single flood events, i.e.
   * events with no annuality)
   * 
   * @see org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection#getReturnPeriod()
   */
  @Override
  public Integer getReturnPeriod( )
  {
    final Integer integer = (Integer) getProperty( IAnnualCoverageCollection.PROP_ANNUALITY );
    if( integer == null )
      return 1;

    return integer;
  }

  /**
   * Sets the return period; the default value is 1 (support for single flood events, i.e. events with no annuality)
   * 
   * @see org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection#setReturnPeriod(java.lang.Integer)
   */
  @Override
  public void setReturnPeriod( final Integer value )
  {
    setProperty( IAnnualCoverageCollection.PROP_ANNUALITY, (value == null || value.intValue() < 1) ? 1 : value.intValue() ); //$NON-NLS-1$
  }
}