/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.optimize.transform;

import org.kalypso.optimizer.Parameter;

/**
 * data transfer object of a parameter optimization configuration
 * 
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

  public ParameterOptimizeContext( final double initialValue, final double syntecticValue, final double lowerBound, final double upperBound,
      final String mode, final String[] xPaths )
  {
    m_initialValue = initialValue;
    m_synteticValue = syntecticValue;
    m_lowerBound = lowerBound;
    m_upperBound = upperBound;
    m_mode = mode;
    m_xPaths = xPaths;
  }

  public ParameterOptimizeContext( final Parameter parameter )
  {
    m_initialValue = parameter.getInitialValue();
    m_synteticValue = parameter.getSynteticValue();
    m_lowerBound = parameter.getLowerBound();
    m_upperBound = parameter.getUpperBound();
    m_mode = parameter.getMode();

    final String[] pathes = parameter.getXpath().toArray( new String[parameter.getXpath().size()] );
    m_xPaths = new String[pathes.length];
    for( int i = 0; i < pathes.length; i++ )
      m_xPaths[i] = pathes[i];
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