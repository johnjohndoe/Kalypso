package org.kalypso.services.calculation.common;

/**
 * @author belger
 */
public interface ICalcServiceConstants
{
  public final int CANCELED = 2;
  public final int ERROR = 4;
  public final int FINISHED = 1;
  public final int RUNNING = 0;
  public final int UNKNOWN = -1;
  public final int WAITING = 3;
  public final int WAITING_FOR_DATA = 5;

  public final String INPUT_DIR_NAME = "input";
  public final String OUTPUT_DIR_NAME = "output";
  public final String RESULT_DIR_NAME = "Ergebnisse";
  public final String CALC_DIR_NAME = "calc";
}
