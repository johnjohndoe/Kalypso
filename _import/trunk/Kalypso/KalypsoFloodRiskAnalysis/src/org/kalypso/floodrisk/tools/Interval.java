package org.kalypso.floodrisk.tools;

/**
 * 
 * Interval
 * <p>
 * 
 * created by
 * 
 * @author Nadja Peiler (17.06.2005)
 */
public class Interval
{

  private double lowerLimit;

  private double upperLimit;

  /**
   * constructs an Interval with the given lower and upper limit
   * 
   * @param lowerLimit
   * @param upperLimit
   */
  public Interval( double lowerLimit, double upperLimit )
  {
    this.lowerLimit = lowerLimit;
    this.upperLimit = upperLimit;
  }

  /**
   * checks if the Interval contains the value x
   * 
   * @param x
   * @return true, if Interval contains the value; otherwise false
   */
  public boolean contains( double x )
  {
    return ( ( lowerLimit <= x ) && ( x <= upperLimit ) );
  }

  /**
   * @return Returns the lowerLimit.
   */
  public double getLowerLimit()
  {
    return lowerLimit;
  }

  /**
   * @return Returns the upperLimit.
   */
  public double getUpperLimit()
  {
    return upperLimit;
  }

  /**
   * @param lowerLimit
   *          The lowerLimit to set.
   */
  public void setLowerLimit( double lowerLimit )
  {
    this.lowerLimit = lowerLimit;
  }

  /**
   * @param upperLimit
   *          The upperLimit to set.
   */
  public void setUpperLimit( double upperLimit )
  {
    this.upperLimit = upperLimit;
  }
}