package org.kalypso.services.calcjob;

import java.net.URL;


/**
 * @author belger
 */
public interface CalcJob extends Runnable
{
  public void init( final String id, final String description, final URL[] arguments, final String type )
      throws CalcJobServiceException;

  public CalcJobDescription getDescription();

  public URL[] getResults();
  
  /** Alles freigeben und evtl. temporäre Dateien löschen (z.B: die URL von getResult) */
  public void disposeJob();
}