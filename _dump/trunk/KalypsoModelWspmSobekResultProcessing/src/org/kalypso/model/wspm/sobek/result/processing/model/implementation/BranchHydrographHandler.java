package org.kalypso.model.wspm.sobek.result.processing.model.implementation;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.runtime.Assert;
import org.kalypso.model.wspm.sobek.result.processing.model.IBranchHydrograph;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.feature.Feature;

public class BranchHydrographHandler extends AbstractFeatureWrapper implements IBranchHydrograph
{

  public BranchHydrographHandler( final Feature feature )
  {
    super( feature );
    Assert.isNotNull( feature );
  }

  public String getBranchId( )
  {
    return (String) getProperty( QN_BRANCH );
  }

  public String getName( )
  {
    return (String) getProperty( QN_NAME );
  }

  public IObservation<TupleResult> getObservation( )
  {
    throw new NotImplementedException();
  }

  public String getParameterId( )
  {
    return (String) getProperty( QN_PARAM_ID );
  }

  public String getUnit( )
  {
    return (String) getProperty( QN_UNIT );
  }

}
