package org.kalypso.model.wspm.sobek.calculation.job.test;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.simulation.core.ISimulationMonitor;

public class TestCaseSimulationMonitor implements ISimulationMonitor
{

  public int getFinishStatus( )
  {
    return 0;
  }

  public String getFinishText( )
  {
    throw new NotImplementedException();
  }

  public String getMessage( )
  {
    throw new NotImplementedException();
  }

  public int getProgress( )
  {
    return 0;
  }

  public void setFinishInfo( final int status, final String text )
  {
    System.out.println( String.format( "ISimualionMonitor.setFinishInfo() - id: %d\ntext: %s", status, text ) );
  }

  public void setMessage( final String message )
  {
    System.out.println( String.format( "ISimualionMonitor.setMessage() - message: %s", message ) );
  }

  public void setProgress( final int progress )
  {
    System.out.println( String.format( "ISimualionMonitor.setProgress() - progress: %d", progress ) );

  }

  public void cancel( )
  {

  }

  public boolean isCanceled( )
  {
    return false;
  }

}
