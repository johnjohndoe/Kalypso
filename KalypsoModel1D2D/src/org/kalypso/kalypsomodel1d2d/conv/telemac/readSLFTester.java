/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.conv.telemac;

import static org.junit.Assert.*;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URISyntaxException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.vfs2.FileSystemException;
import org.deegree.framework.util.Pair;
import org.junit.Test;

/**
 * @author ilya
 *
 */
public class readSLFTester
{

  @Test
  public void test( )
  {
    final SerafinReader convTelemac = new SerafinReader();
//    convTelemac.setFile( new File( "D:\\ig_share\\telemac\\018_culm\\r2d_culm.slf" ) );
//    convTelemac.setFile( new File( "D:\\ig_share\\telemac\\014_cavity\\r2d_cavity_v1p0.slf.old" ) );
    PrintWriter out = null;
    try
    {
      out = new PrintWriter("D:\\ig_share\\telemac\\014_cavity\\filenameLog.txt");
    }
    catch( FileNotFoundException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
      System.out.println("log error!");
    }
    System.out.println("setup!");
    Map<Integer, Pair<String, Map<String, List<Double>>>> mapResults = null;
    try
    {
      mapResults = convTelemac.doReadAll();
    }
    catch( IOException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    System.out.println("read done!");
//    for( Iterator iterator = mapResults.keySet().iterator(); iterator.hasNext(); )
    for( int i = 0; i < mapResults.size(); ++i )
    {
      Map<String, List<Double>> m = mapResults.get( i ).second;
      for( Iterator iterator2 = m.keySet().iterator(); iterator2.hasNext(); )
      {
        String next = (String)iterator2.next();
        List< Double > listV = m.get( next );
        out.println( "next" + next );
//        System.out.println( "next" + next  );
        for( Iterator iterator3 = listV.iterator(); iterator3.hasNext(); )
        {
          Double double1 = (Double)iterator3.next();
//          System.out.print( " " + double1 );
          out.println( " " + double1 );
          
        }
          
      }
      
    }
    System.out.println("exit...");
//    fail( "Not yet implemented" );
  }

}
