package org.kalypso.services.calculation.job;

import java.io.File;

import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * @author belger
 */
public interface ICalcJob
{
  public void run( final File basedir, final CalcJobDataBean[] input ) throws CalcJobServiceException;
  
  /**
   * Alles freigeben und evtl. temporäre Dateien löschen (z.B: die URL von
   * getResult)
   */
  public void disposeJob();

  /**
   * Bricht den Job ab, er sollte so schnell wie möglich die <code>run</code>
   * Methode verlassen
   */
  public void cancel();
  
  public boolean isCanceled();
  
  /** Gibt den aktuellen Fortschritt des Jobs zurück, zwischen 0 und 100*/
  public int getProgress();
  
  public String getMessage();
  
  public CalcJobDataBean[] getResults();
}