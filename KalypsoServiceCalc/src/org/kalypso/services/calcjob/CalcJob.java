package org.kalypso.services.calcjob;




/**
 * @author belger
 */
public interface CalcJob extends Runnable
{
  public void init( final String id, final String description, final String[] arguments, final String type )
      throws CalcJobServiceException;

  public CalcJobDescription getDescription();

  public String[] getResults();
  
  /** Alles freigeben und evtl. temporäre Dateien löschen (z.B: die URL von getResult) */
  public void disposeJob();
}