package org.kalypso.model.wspm.sobek.calculation.job.test;

import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

public class TestCaseResultEater implements ISimulationResultEater
{

  public void addResult( final String id, final Object result ) throws SimulationException
  {
    System.out.println( String.format( "Adding result %s, type of %s", id, result.getClass().toString() ) );
  }

}
