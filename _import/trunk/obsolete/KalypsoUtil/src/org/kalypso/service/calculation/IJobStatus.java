package org.kalypso.service.calculation;

/**
 * @author gernot
 */
public interface IJobStatus
{
  public boolean isFinished();
  
  /** in Prozent */
  public double progress();
}
