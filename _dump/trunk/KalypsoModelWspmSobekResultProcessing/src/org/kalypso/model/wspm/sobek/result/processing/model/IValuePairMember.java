package org.kalypso.model.wspm.sobek.result.processing.model;

import javax.xml.namespace.QName;

import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;

public interface IValuePairMember
{
  public static final QName QN_TYPE = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "DurationValuePair" );

  public static final QName QN_TIME_PERIODE = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "timePeriod" );

  public static final QName QN_WATERLEVEL = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "value" );

}
