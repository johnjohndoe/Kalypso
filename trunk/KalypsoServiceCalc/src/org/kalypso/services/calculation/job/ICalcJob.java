package org.kalypso.services.calculation.job;

import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * @author belger
 */
public interface ICalcJob
{
  public void run( final CalcJobDataBean[] input ) throws CalcJobServiceException;
  
  /**
   * Alles freigeben und evtl. tempor�re Dateien l�schen (z.B: die URL von
   * getResult)
   */
  public void disposeJob();

  /**
   * Bricht den Job ab, er sollte so schnell wie m�glich die <code>run</code>
   * Methode verlassen
   */
  public void cancel();
  
  public boolean isCanceled();
  
  /** Gibt den aktuellen Fortschritt des Jobs zur�ck, zwischen 0 und 100*/
  public int getProgress();
  
  public String getMessage();
  
  public CalcJobDataBean[] getResults();
}