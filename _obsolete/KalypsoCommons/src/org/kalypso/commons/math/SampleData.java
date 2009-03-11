/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.commons.math;

import java.util.Date;
import java.util.TreeMap;

/**
 * @author alex_burtscher
 * Klasse zum Erzeugen von Beispieldaten für das Chart
 * 
 * 
 */
public class SampleData
{
  public static Double[][] createSinusPoints( final int size )
  {
    final Double[][] data = new Double[size][2];
    for( int i = 0; i < data.length; i++ )
    {
      data[i][0] = new Double(1+ (100/data.length) * i );
      data[i][1] = ((double) i + 1) / data.length * Math.sin( 16 * Math.PI / size * i/2 );
    }
    return data;
  }

  public static Double[][] createRandomPoints( int size )
  {
    final Double[][] data = new Double[size][2];
    for( int i = 0; i < data.length; i++ )
    {
      data[i][0] = new Double(1+ (100/data.length) * i );
      data[i][1] = new Double( (int) (Math.random() * 10) );
    }
    return data;
  }

  public static TreeMap<Date, Double> createRandomDatePoints( int size )
  {
    final TreeMap<Date, Double> data = new TreeMap<Date, Double>();
    
    long now=System.currentTimeMillis();
    
    long dayInMillis=1000*60*60*24;
    
    long start=now-size*dayInMillis;
    
    for( int i = 0; i < size; i++ )
    {
      Date date=new Date(start+i*dayInMillis);
      Double val=new Double( (int) (Math.random() * 10) );
      data.put(date, val);
    }
    return data;
  }
}
