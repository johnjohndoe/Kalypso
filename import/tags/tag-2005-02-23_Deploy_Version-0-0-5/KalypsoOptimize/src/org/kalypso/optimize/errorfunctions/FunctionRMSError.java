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
import java.util.Iterator;
import java.util.TreeMap;

/**
 * @author doemming
 */
public class FunctionRMSError extends IErrorFunktion
{
  
  public FunctionRMSError( TreeMap measuredTS, Date startCompare, Date endCompare, double errorDivisor )
  {
    super( measuredTS, startCompare, endCompare, errorDivisor );
    
  }
  public double calculateError( TreeMap calced)
  {
    double error = 0;
    double c = 0;
    Iterator it_all = calced.keySet().iterator();
    while( it_all.hasNext() )
    {
      Date dateKey = (Date)it_all.next();
      if( m_startCompare.before( dateKey ) && m_endCompare.after( dateKey ) )
      {
        try
        {
          final double valueCalced = ( (Double)calced.get( dateKey ) ).doubleValue();
          final double valueMeasured = ( (Double)m_measuredTS.get( dateKey ) ).doubleValue();
          error += Math.pow( valueCalced - valueMeasured, 2d );
          c++;
        }
        catch( Exception e )
        {
          e.printStackTrace();  
        }
      }
    }
    return Math.sqrt( error / c )/m_errorDivisor;
  }
}