package org.kalypso.services.calculation.common;

import org.kalypso.services.calculation.service.CalcJobBean;

/**
 * @author belger
 */
public class CalcJobHelper
{
  private CalcJobHelper()
  {
    // soll nicht initialiser werden
  }
  
  public static CalcJobBean createCalcJobBean( final ICalcJobInfo info )
  {
    return new CalcJobBean( info.getId(), info.getDescription(), info.getType(), info.getState(), info.getProgress(), info.getResults() );
  }
}
