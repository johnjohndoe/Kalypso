package org.kalypso.optimize.errorfunctions;

import java.util.Date;
import java.util.TreeMap;

/**
 * @author doemming
 */
public abstract class IErrorFunktion
{
  protected final Date m_startCompare;

  protected final Date m_endCompare;

  protected final TreeMap m_measuredTS;

  protected double m_errorDivisor;

  /**
   * abstract error funktion class
   * 
   * @param errorDivisor
   *          calculated errors can be weighted with this divisor, this makes it
   *          possible to compare different errorfuntions, good idea is to start
   *          with 1.0d and update it later
   */
  public IErrorFunktion( TreeMap measuredTS, Date startCompare, Date endCompare, double errorDivisor )
  {
    m_measuredTS = measuredTS;
    m_startCompare = startCompare;
    m_endCompare = endCompare;
    m_errorDivisor = errorDivisor;
  }

  public abstract double calculateError( TreeMap calcedTS );

  public double updateErrorDivisor( TreeMap calcedTS )
  {
    m_errorDivisor=calculateError(calcedTS);
    return m_errorDivisor;
  }
}