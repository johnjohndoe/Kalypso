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