package org.kalypso.services.calculation;




/**
 * @author belger
 */
public interface ICalcJob extends Runnable
{
  public void init( final String id, final String description, final String[] arguments, final String type )
      throws CalcJobServiceException;

  public CalcJobBean getJobBean();

  /** Alles freigeben und evtl. temporäre Dateien löschen (z.B: die URL von getResult) */
  public void disposeJob();
}