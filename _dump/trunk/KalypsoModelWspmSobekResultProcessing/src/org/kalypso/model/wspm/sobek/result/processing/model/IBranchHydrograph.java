package org.kalypso.model.wspm.sobek.result.processing.model;

import javax.xml.namespace.QName;

import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.feature.Feature;

public interface IBranchHydrograph extends Feature
{
  public static final QName QN_TYPE = new QName( ISobekConstants.NS_SOBEK_RESULT_BRANCH_HYDROGRAPHS, "ResultLs" );

  public static final QName QN_BRANCH = new QName( ISobekConstants.NS_SOBEK_RESULT_BRANCH_HYDROGRAPHS, "ResultLs" );

  public static final QName QN_NAME = new QName( ISobekConstants.NS_SOBEK_RESULT_BRANCH_HYDROGRAPHS, "ResultLs" );

  public static final QName QN_PARAM_ID = new QName( ISobekConstants.NS_SOBEK_RESULT_BRANCH_HYDROGRAPHS, "ResultLs" );

  public static final QName QN_UNIT = new QName( ISobekConstants.NS_SOBEK_RESULT_BRANCH_HYDROGRAPHS, "ResultLs" );

  public static final QName QN_RESULT_MEMBER = new QName( ISobekConstants.NS_SOBEK_RESULT_BRANCH_HYDROGRAPHS, "ResultLs" );

// - {org.kalypso.model.wspm.sobek.result.ls}resultMember

  public String getBranchId( );

  public String getName( );

  public String getParameterId( );

  public String getUnit( );

  public IObservation<TupleResult> getObservation( );
}
