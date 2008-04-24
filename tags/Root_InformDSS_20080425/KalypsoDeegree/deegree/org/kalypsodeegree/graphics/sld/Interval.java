package org.kalypsodeegree.graphics.sld;

/**
 * @author N. Peiler
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public interface Interval
{

  double getLowerLimit();

  void setLowerLimit( double lowerLimit );

  double getUpperLimit();

  void setUpperLimit( double upperLimit );

  boolean contains( double x );
}
