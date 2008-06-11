package org.kalypso.model.wspm.sobek.result.processing.model.implementation;

import javax.xml.datatype.Duration;

import org.kalypso.model.wspm.sobek.result.processing.model.IValuePairMember;
import org.kalypsodeegree.model.feature.Feature;

public class ValuePairMemberHandler extends AbstractFeatureWrapper implements IValuePairMember, Feature
{

  public ValuePairMemberHandler( final Feature feature )
  {
    super( feature );
  }

  public Duration getTimePeriod( )
  {
    return (Duration) getProperty( QN_TIME_PERIODE );
  }

  public Double getWaterLevel( )
  {
    return (Double) getProperty( QN_WATERLEVEL );
  }

}
