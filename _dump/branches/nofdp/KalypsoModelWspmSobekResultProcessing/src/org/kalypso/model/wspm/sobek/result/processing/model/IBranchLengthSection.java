package org.kalypso.model.wspm.sobek.result.processing.model;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.feature.Feature;

public interface IBranchLengthSection extends Feature
{
  public static final QName QN_TYPE = new QName( ISobekConstants.NS_SOBEK_RESULT_BRANCH_HYDROGRAPHS, "ResultLs" );

  public static final QName QN_BRANCH = new QName( ISobekConstants.NS_SOBEK_RESULT_BRANCH_HYDROGRAPHS, "branchID" );

  public static final QName QN_NAME = new QName( ISobekConstants.NS_SOBEK_RESULT_BRANCH_HYDROGRAPHS, "name" );

  public static final QName QN_PARAM_ID = new QName( ISobekConstants.NS_SOBEK_RESULT_BRANCH_HYDROGRAPHS, "paramID" );

  public static final QName QN_UNIT = new QName( ISobekConstants.NS_SOBEK_RESULT_BRANCH_HYDROGRAPHS, "unit" );

  public static final QName QN_RESULT_MEMBER = new QName( ISobekConstants.NS_SOBEK_RESULT_BRANCH_HYDROGRAPHS, "resultMember" );

// - {org.kalypso.model.wspm.sobek.result.ls}resultMember

  public static final String DICT_OBS_STATION = "urn:ogc:gml:dict:kalypso:wspm:sobek:resultLengthSectionObservationDefs#STATION"; //$NON-NLS-1$

  public static final String DICT_OBS_WATERLEVEL = "urn:ogc:gml:dict:kalypso:wspm:sobek:resultLengthSectionObservationDefs#WATERLEVEL"; //$NON-NLS-1$

  public static final String DICT_OBS_DISCHARGE = "urn:ogc:gml:dict:kalypso:wspm:sobek:resultLengthSectionObservationDefs#DISCHARGE"; //$NON-NLS-1$

  public static final String DICT_OBS_LABEL = "urn:ogc:gml:dict:kalypso:wspm:sobek:resultLengthSectionObservationDefs#LABEL"; //$NON-NLS-1$

  public static final String DICT_OBS_NODE_TYPE = "urn:ogc:gml:dict:kalypso:wspm:sobek:resultLengthSectionObservationDefs#NODETYPE"; //$NON-NLS-1$

  public String getBranchId( );

  public String getName( );

  public String getParameterId( );

  public String getUnit( );

  public IObservation<TupleResult> getObservation( ) throws CoreException;
}
