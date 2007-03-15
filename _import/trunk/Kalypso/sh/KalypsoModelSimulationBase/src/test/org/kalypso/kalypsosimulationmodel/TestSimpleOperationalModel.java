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

import java.io.OutputStreamWriter;
import java.net.URL;

import junit.framework.TestCase;

import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessPolygon;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class TestSimpleOperationalModel extends TestCase
{

  public void testOutput( )
  {
    GMLWorkspace workspace = null;
    URL modelURL = TestSimpleOperationalModel.class.getResource( "data/simulation_model_operational.xml" );
    try
    {
      workspace = GmlSerializer.createGMLWorkspace( modelURL, null );
      Feature rootFeature = workspace.getRootFeature();

      Feature rp = FeatureHelper.addFeature( rootFeature, TestWorkspaces.GML_PROP_FEATURE_MEMBER, KalypsoModelSimulationBaseConsts.SIM_BASE_F_SIMPLE_OPERATIONAL_MODEL );

      // gm pos
      GM_Position exteriorRing[] = new GM_Position[5];

      double[][] posArray = { { 0, 0 }, { 0, 1 }, { 1, 1 }, { 1, 0 }, { 0, 0 } };
      for( int i = 0; i < 5; i++ )
      {
        exteriorRing[i] = GeometryFactory.createGM_Position( posArray[i] );
      }

//      rp.setProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_P_OPERATIONAL_CONCEPT, surface );

      rp.setProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_P_POINT_COVERAGE, "htpp://wwww.tuhh.de/wb/roughness_db/grass" );

      System.out.println( "=====================================" );
      OutputStreamWriter writer = new OutputStreamWriter( System.out );
      GmlSerializer.serializeWorkspace( writer, workspace );

    }
    catch( Throwable th )
    {
      fail( TestUtils.getStackTraceAsString( th ) );
    }

  }

  public void testWorkspaceLoad( )
  {

    GMLWorkspace workspace = null;

    try
    {
//      workspace = GmlSerializer.createGMLWorkspace( TestWorkspaces., null );
    }
    catch( Throwable th )
    {
      fail( TestUtils.getStackTraceAsString( th ) );
    }
//    Feature rFeature = workspace.getRootFeature();
//    FeatureList list = (FeatureList) rFeature.getProperty( TestWorkspaces.GML_PROP_FEATURE_MEMBER );
//    RoughnessPolygon rp = new RoughnessPolygon( (Feature) list.get( 0 ) );
//    assertEquals( "htpp://wwww.tuhh.de/wb/roughness_db/grass", rp.getRoughnessStyle() );
//    GM_Polygon pol = rp.getPolygon();
//    GM_Position[] positions = pol.getExteriorRing();
//    double[][] posArray = { { 0, 0 }, { 0, 1 }, { 1, 1 }, { 1, 0 }, { 0, 0 } };
//
//    int i = 0;
//    for( GM_Position pos : positions )
//    {
//      assertTrue( Arrays.equals( posArray[i], pos.getAsArray() ) );
//      i++;
//    }

  }
}
