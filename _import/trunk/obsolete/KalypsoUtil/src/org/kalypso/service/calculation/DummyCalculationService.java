package org.kalypso.service.calculation;

/**
 * @author gernot
 */
public class DummyCalculationService implements ICalculationService
{
  /**
   * @see org.kalypso.service.calculation.ICalculationService#getCalcTypes()
   */
  public String[] getCalcTypes()
  {
    return new String[] { "dummy" };
  }

  /**
   * @see org.kalypso.service.calculation.ICalculationService#startCalcJob(java.lang.String, java.lang.String[])
   */
  public String startCalcJob( final String typer, final String[] data )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.service.calculation.ICalculationService#getJobStatus(java.lang.String)
   */
  public IJobStatus getJobStatus( String jobID )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.service.calculation.ICalculationService#getJobResults(java.lang.String)
   */
  public String[] getJobResults( String jobID )
  {
    // TODO Auto-generated method stub
    return null;
  }

}
