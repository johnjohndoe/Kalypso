package org.kalypso.services.calculation.job;

import org.kalypso.services.calculation.common.ICalcJobInfo;
import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * @author belger
 */
public interface ICalcJob extends Runnable, ICalcJobInfo
{
  public void init( final String id, final String type, final String description, final CalcJobDataBean[] input ) throws CalcJobServiceException;

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

  /**
   * Signalisiert, dass der Job jetzt starten k�nnte, alle Eingabedateien sind jetzt vorhanden
   * @throws CalcJobServiceException
   */
  public void setReady() throws CalcJobServiceException;
}