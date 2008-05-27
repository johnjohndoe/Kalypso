package org.kalypso.model.wspm.sobek.result.processing.model.implementation;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.model.wspm.sobek.result.processing.model.IResultTimeSeries;
import org.kalypso.model.wspm.sobek.result.processing.model.IValuePairMembers;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.feature.Feature;

public class ResultTimeSeriesHandler extends AbstractFeatureWrapper implements IResultTimeSeries
{
  public ResultTimeSeriesHandler( final Feature feature )
  {
    super( feature );
  }

  public String getBranchId( )
  {
    // FIXME
    throw new NotImplementedException();
  }

  public Double getLastValue( )
  {
    // FIXME
    throw new NotImplementedException();
  }

  public Double getMaxValue( )
  {
    // FIXME
    throw new NotImplementedException();
  }

  public String getName( )
  {
    // FIXME
    throw new NotImplementedException();
  }

  public IObservation<TupleResult> getObservation( )
  {
    // FIXME
    throw new NotImplementedException();
  }

  public String getParameterId( )
  {
    // FIXME
    throw new NotImplementedException();
  }

  public Double getStationBranchPosition( )
  {
    // FIXME
    throw new NotImplementedException();
  }

  public String getStationName( )
  {
    // FIXME
    throw new NotImplementedException();
  }

  public String getUniqueId( )
  {
    // FIXME
    throw new NotImplementedException();
  }

  public String getUnit( )
  {
    // FIXME
    throw new NotImplementedException();
  }

  public IValuePairMembers getValuePairs( )
  {
    // FIXME
    throw new NotImplementedException();
  }

}
