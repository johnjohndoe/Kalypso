package org.kalypso.model.wspm.sobek.result.processing.model;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;

public interface IResultTimeSeries extends Feature
{

  public static final QName QN_TYPE = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "ResultTs" ); //$NON-NLS-1$

  public static final QName QN_UNIQUE_ID = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "uniqueID" ); //$NON-NLS-1$

  public static final QName QN_STATION_NAME = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "stationName" ); //$NON-NLS-1$

  public static final QName QN_NAME = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "name" ); //$NON-NLS-1$

  public static final QName QN_PARAM_ID = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "paramID" ); //$NON-NLS-1$

  public static final QName QN_UNIT = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "unit" ); //$NON-NLS-1$

  public static final QName QN_MAX_VALUE = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "maxValue" ); //$NON-NLS-1$

  public static final QName QN_LAST_VALUE = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "lastValue" ); //$NON-NLS-1$

  public static final QName QN_VALUE_PAIRS = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "durationValuePairMember" ); //$NON-NLS-1$

  public static final QName QN_RESULT_MEMBER = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "resultMember" ); //$NON-NLS-1$

  public static final String DICT_OBS_DATE = "urn:ogc:gml:dict:kalypso:wspm:sobek:resultTimeSeriesObservationDefs#DATE"; //$NON-NLS-1$

  public static final String DICT_OBS_WATERLEVEL = "urn:ogc:gml:dict:kalypso:wspm:sobek:resultTimeSeriesObservationDefs#WATERLEVEL"; //$NON-NLS-1$

  public static final String DICT_OBS_DISCHARGE = "urn:ogc:gml:dict:kalypso:wspm:sobek:resultTimeSeriesObservationDefs#DISCHARGE"; //$NON-NLS-1$

  public static final String DICT_OBS_INFLOW = "urn:ogc:gml:dict:kalypso:wspm:sobek:resultTimeSeriesObservationDefs#INFLOW"; //$NON-NLS-1$

  public static final String DICT_OBS_OUTFLOW = "urn:ogc:gml:dict:kalypso:wspm:sobek:resultTimeSeriesObservationDefs#OUTFLOW"; //$NON-NLS-1$

  public static final String DICT_OBS_GRUNDABLASS = "urn:ogc:gml:dict:kalypso:wspm:sobek:resultTimeSeriesObservationDefs#GRUNDABLASS"; //$NON-NLS-1$

  public static final String DICT_OBS_HOCHWASSERENTLASTUNG = "urn:ogc:gml:dict:kalypso:wspm:sobek:resultTimeSeriesObservationDefs#HOCHWASSERENTLASTUNG"; //$NON-NLS-1$

  public static final QName QN_MIN_VALUE = new QName( ISobekConstants.NS_SOBEK_RESULT_TIME_SERIES, "minValue" ); //$NON-NLS-1$

  public String getUniqueId( );

  public String getStationName( );

  public String getName( );

  public String getParameterId( );

  public String getUnit( );

  public Double getPositionOnBranch( IBranch branch ) throws GM_Exception, CoreException;

  public Double getMaxValue( );

  public Double getMinValue( );

  public Double getLastValue( );

  IValuePairMembers getValuePairs( );

  IObservation<TupleResult> getObservation( ) throws CoreException;
}
