package org.kalypso.optimize.transform;

import org.kalypso.optimizer.Parameter;

/**
 * data transfer object of a parameter optimization configuration
 * @author doemming
 */

public class ParameterOptimizeContext
{
  public final static String MODE_FACTOR = "factor";

  public final static String MODE_OFFSET = "offset";

  public final static String MODE_DIRECT = "direct";
  
  public final static String MODE_INITIAL = "setInitial";

  private final double m_upperBound;

  private final double m_lowerBound;

  private final double m_initialValue;

  private final double m_synteticValue;

  private final String m_mode;

  private final String[] m_xPaths;

  public ParameterOptimizeContext( double initialValue, double syntecticValue, double lowerBound,
      double upperBound, String mode, String[] xPaths )
  {
    m_initialValue = initialValue;
    m_synteticValue = syntecticValue;
    m_lowerBound = lowerBound;
    m_upperBound = upperBound;
    m_mode = mode;
    m_xPaths = xPaths;
  }

  public ParameterOptimizeContext( Parameter parameter )
  {
    m_initialValue = parameter.getInitialValue();
    m_synteticValue = parameter.getSynteticValue();
    m_lowerBound = parameter.getLowerBound();
    m_upperBound = parameter.getUpperBound();
    m_mode = parameter.getMode();
    m_xPaths = (String[])parameter.getXpath().toArray(new String[parameter.getXpath().size()]);
  }

  public double getUpperBound()
  {
    return m_upperBound;
  }

  public double getLowerBound()
  {
    return m_lowerBound;
  }

  public double getInitialValue()
  {
    return m_initialValue;
  }

  public double getSynteticValue()
  {
    return m_synteticValue;
  }

  public String getMode()
  {
    return m_mode;
  }

  public String[] getxPaths()
  {
    return m_xPaths;
  }
}