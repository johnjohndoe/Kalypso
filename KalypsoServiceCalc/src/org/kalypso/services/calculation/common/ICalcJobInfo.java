package org.kalypso.services.calculation.common;

import org.kalypso.services.calculation.service.CalcJobDataBean;

/**
 * Interface for Information-Retrieval from {@link org.kalypso.services.calculation.job.ICalcJob}
 * 
 * @author belger
 */
public interface ICalcJobInfo
{
  public final static int UNKNOWN = -1;
  public final static int RUNNING = 0;
  public final static int FINISHED = 1;
  public final static int CANCELED = 2;
  public final static int WAITING = 3;
  public final static int ERROR = 4;
  public final static int WAITING_FOR_DATA = 5;

  /** Die ID des Jobs */
  public String getId();

  /** Der Rechentyp des Jobs */
  public String getType();

  /** Die Beschreibung des Jobs */
  public String getDescription();

  /** Der Zustand des Jobs, eine der oben definierten Konstanten  */
  public int getState();

  /** Der Fortschritt des Jobs, zwischen 0 und 100 */
  public int getProgress();

  /** <p>Eine Meldung über den aktuellen Verlauf der Rechnung.</p>
   * <p>Falls es einen Fehler gab, steht hier die Fehlermeldung</p> 
   */
  public String getMessage();

  /** Die bereits vorhandenen Ergebnisse */
  public CalcJobDataBean[] getResults();
}
