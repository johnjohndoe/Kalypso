/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import junit.framework.TestCase;

import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.TerrainElevationModelSystem;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Madanagopal
 * @author Patrice Congo
 *
 */
public class TestTerrainElevationModelSystem extends TestCase
{

  public void testWorkspaceLoad()
  {
          
      GMLWorkspace workspace=null;
      
      try
      {
          workspace=
              GmlSerializer.createGMLWorkspace( 
                              TestWorkspaces.URL_TEM_SYSREM, 
                              null );
          Feature rcFeature=workspace.getRootFeature();
//          ITerrainElevationModelSystem temSystem=
//              (ITerrainElevationModelSystem)rcFeature.getAdapter( 
//                                              ITerrainElevationModelSystem.class );
          //TODO test  with adapater
          ITerrainElevationModelSystem temSystem=
            new TerrainElevationModelSystem(rcFeature);
          assertNotNull( 
              Messages.getString("TestTerrainElevationModelSystem.0")+ITerrainElevationModelSystem.class, //$NON-NLS-1$
              temSystem );
          
          
          for(int i=0;i<10;i++)
          {
            for(int j=0;j<10;j++)
            {
              double jFlip=9-j;
              double x=5*i+1+13;
              double y=5*jFlip+1+154;
              if(i==j)
              {
                GM_Point curPoint = 
                  GeometryFactory.createGM_Point( 
                    x, y, TestWorkspaces.getGaussKrueger() );
                double ele=temSystem.getElevation( curPoint  );
                assertEquals(
                    "i="+i+" j="+j+" ele="+ele,i*j*1.000,ele); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                
              }
              else
              {
                GM_Point curPoint = 
                  GeometryFactory.createGM_Point( 
                    x,y, TestWorkspaces.getGaussKrueger() );
                double ele=temSystem.getElevation( curPoint  );
                assertEquals(
                    "i="+i+" j="+j+" ele="+ele,Double.NaN,ele); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
              }
            }
          }
      }
      catch (Throwable th) 
      {
        fail(TestUtils.getStackTraceAsString( th ));
      }
  }
  
}
