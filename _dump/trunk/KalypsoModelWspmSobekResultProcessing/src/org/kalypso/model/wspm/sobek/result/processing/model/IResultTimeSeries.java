package org.kalypso.model.wspm.sobek.result.processing.model;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.feature.Feature;

public interface IResultTimeSeries extends Feature
{
// {org.kalypso.model.wspm.sobek.result.ts}ResultTs
// - {org.kalypso.model.wspm.sobek.result.ts}uniqueID
// - {org.kalypso.model.wspm.sobek.result.ts}stationName
// - {org.kalypso.model.wspm.sobek.result.ts}name
// - {org.kalypso.model.wspm.sobek.result.ts}paramID
// - {org.kalypso.model.wspm.sobek.result.ts}unit
// - {org.kalypso.model.wspm.sobek.result.ts}stationAlongBranch
// - {org.kalypso.model.wspm.sobek.result.ts}branchID
// - {org.kalypso.model.wspm.sobek.result.ts}maxValue
// - {org.kalypso.model.wspm.sobek.result.ts}lastValue
// - {org.kalypso.model.wspm.sobek.result.ts}durationValuePairMember
// - {org.kalypso.model.wspm.sobek.result.ts}resultMember

  public static final QName QN_TYPE = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "ResultTs" );

  public static final QName QN_UNIQUE_ID = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "uniqueID" );

  public static final QName QN_STATION_NAME = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "stationName" );

  public static final QName QN_NAME = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "name" );

  public static final QName QN_PARAM_ID = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "paramID" );

  public static final QName QN_UNIT = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "unit" );

  public static final QName QN_STATION_BRANCH_POSITION = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "stationAlongBranch" );

  public static final QName QN_BRANCH_ID = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "branchID" );

  public static final QName QN_MAX_VALUE = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "maxValue" );

  public static final QName QN_LAST_VALUE = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "lastValue" );

  public static final QName QN_VALUE_PAIRS = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "durationValuePairMember" );

  public static final QName QN_RESULT_MEMBER = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "resultMember" );

  public static final String DICT_OBS_DATE = "urn:ogc:gml:dict:kalypso:wspm:sobek:resultTimeSeriesObservationDefs#DATE"; //$NON-NLS-1$

  public static final String DICT_OBS_WATERLEVEL = "urn:ogc:gml:dict:kalypso:wspm:sobek:resultTimeSeriesObservationDefs#WATERLEVEL"; //$NON-NLS-1$

  public String getUniqueId( );

  public String getStationName( );

  public String getName( );

  public String getParameterId( );

  public String getUnit( );

  public Double getStationBranchPosition( );

  public String getBranchId( );

  public Double getMaxValue( );

  public Double getLastValue( );

  IValuePairMembers getValuePairs( );

  IObservation<TupleResult> getObservation( ) throws CoreException;
}
