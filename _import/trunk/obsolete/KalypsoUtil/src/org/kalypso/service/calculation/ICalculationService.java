package org.kalypso.service.calculation;

/**
 * @author gernot
 */
public interface ICalculationService
{
  public String[] getCalcTypes();
  
  public String startCalcJob( final String typer, final String[] data );
  
  public IJobStatus getJobStatus( final String jobID );
  
  public String[] getJobResults( final String jobID );
}
