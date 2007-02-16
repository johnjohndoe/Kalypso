/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package test.org.kalypso.kalypsosimulationmodel;

import java.io.File;


import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ASCTerrainElevationModel;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

import junit.framework.TestCase;

/**
 * 
 * @author Madanagopal
 * @author Patrice Congo
 *
 */
public class TestASCTerrainElevationProvider extends TestCase
{

  private void makedata()
  {
    double elevationsData[]=
      {
        32144, -9999, -9999, -9999, -9999, -9999, -9999, -9999, -9999, -9999, 
        -9999, 1.000, -9999, -9999, -9999, -9999, -9999, -9999, -9999, -9999, 
        -9999, -9999, 2.000, -9999, -9999, -9999, -9999, -9999, -9999, -9999, 
        -9999, -9999, -9999, 3.000, -9999, -9999, -9999, -9999, -9999, -9999, 
        -9999, -9999, -9999, -9999, 4.000, -9999, -9999, -9999, -9999, -9999, 
        -9999, -9999, -9999, -9999, -9999, 5.000, -9999, -9999, -9999, -9999, 
        -9999, -9999, -9999, -9999, -9999, -9999, 6.000, -9999, -9999, -9999, 
        -9999, -9999, -9999, -9999, -9999, -9999, -9999, 7.000, -9999, -9999, 
        -9999, -9999, -9999, -9999, -9999, -9999, -9999, -9999, 8.000, -9999, 
        -9999, -9999, -9999, -9999, -9999, -9999, -9999, -9999, -9999, -9999};
    for(int i=0;i<10;i++)
    {
      for(int j=0;j<10;j++)
      {
        if(i==j)
        {
          GM_Point point = 
            GeometryFactory.createGM_Point( 
              5*i+1,5*j+1, TestWorkspaces.getGaussKrueger() );
          
        }
      }
    }
    
  }
  
  public void testLoadSmallAscFile()
  {
    try
    {
      
     File ascFile= new File(TestWorkspaces.URL_SMALL_ASC.toURI().normalize().getPath());//);
     // File ascFile = new File("F:\\Eclipse007\\eclipse\\workspace_kaly\\KalypsoModelSimulationBase\\src\\test\\org\\kalypso\\kalypsosimulationmodel\\data\\test_file_small_acs.asc");
      ASCTerrainElevationModel ascModel=
        new ASCTerrainElevationModel(
            TestWorkspaces.URL_SMALL_ASC, null);
      
//      GM_Point point = 
//        GeometryFactory.createGM_Point( 
//          0, 0, TestWorkspaces.getGaussKrueger() );
//      
//      assertEquals(32144.0,ascModel.getElevation( point  ));
//      
//      GM_Point point_2 = 
//        GeometryFactory.createGM_Point( 
//          1, 1, TestWorkspaces.getGaussKrueger() );
//      
//      assertEquals(32144.0,ascModel.getElevation( point_2  ));
      
      for(int i=0;i<10;i++)
      {
        for(int j=0;j<10;j++)
        {
          if(i==j)
          {
            GM_Point curPoint = 
              GeometryFactory.createGM_Point( 
                5*i+1,5*j+1, TestWorkspaces.getGaussKrueger() );
            double ele=ascModel.getElevation( curPoint  );
            assertEquals(
                "i="+i+" j="+j+" ele="+ele,i*j*1.000,ele);
            
          }
          else
          {
            GM_Point curPoint = 
              GeometryFactory.createGM_Point( 
                5*i+1,5*j+1, TestWorkspaces.getGaussKrueger() );
            double ele=ascModel.getElevation( curPoint  );
            assertEquals(
                "i="+i+" j="+j+" ele="+ele,Double.NaN,ele);
          }
        }
      }
      
    }
    catch (Throwable th) 
    {
     fail( TestUtils.getStackTraceAsString( th ) );
    }
  }
}
