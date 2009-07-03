package org.kalypso.convert.namodel.varymodel;

/**
 * @author doemming
 */

public class CalContext
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

  public CalContext( double initialValue, double syntecticValue, double lowerBound,
      double upperBound, String mode, String[] xPaths )
  {
    m_initialValue = initialValue;
    m_synteticValue = syntecticValue;
    m_lowerBound = lowerBound;
    m_upperBound = upperBound;
    m_mode = mode;
    m_xPaths = xPaths;
  }

  //  public void setUpperbound( double value )
  //  {
  //    upperBound = value;
  //  }

  public double getUpperBound()
  {
    return m_upperBound;
  }

  //  public void setLowerbound( double value )
  //  {
  //    lowerBound = value;
  //  }

  public double getLowerBound()
  {
    return m_lowerBound;
  }

  //  public void setInitialValue( double value )
  //  {
  //    initialValue = value;
  //  }

  public double getInitialValue()
  {
    return m_initialValue;
  }

  //  public void setSynteticValue( double value )
  //  {
  //    synteticValue = value;
  //  }

  public double getSynteticValue()
  {
    return m_synteticValue;
  }

  //  public void setMode( String value )
  //  {
  //    mode = value;
  //  }

  public String getMode()
  {
    return m_mode;
  }

  //  public void setXPath( String[] xPaths )
  //  {
  //    xPath = xPaths;
  //  }

  public String[] getxPaths()
  {
    return m_xPaths;
  }

}