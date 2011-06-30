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
package test.org.kalypso.kalypsomodel1d2d;

import junit.framework.TestCase;

import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import test.org.kalypso.kalypsosimulationmodel.TestUtils;

/**
 * @author antanas
 */
public class TestOP extends TestCase
{
  public void testWorkspaceLoad( )
  {

    GMLWorkspace workspace = null;

    try
    {
      workspace = GmlSerializer.createGMLWorkspace( TestOP.class.getResource( "data/sim_1d2d_operational.xml" ), null ); //$NON-NLS-1$
    }
    catch( Throwable th )
    {
      fail( TestUtils.getStackTraceAsString( th ) );
    }
    Feature rFeature = workspace.getRootFeature();
/*    FE1D2DEdge edge = new FE1D2DEdge( rFeature );
    IFeatureWrapperCollection<IFE1D2DElement> container = edge.getContainers();
    assertEquals( 3, container.size() );

    IFE1D2DElement element = container.get( 0 );
    assertEquals( "QuadriElement", element.getWrappedFeature().getId() );
    assertEquals( 4, element.getEdges().size() );

    element = container.get( 1 );
    assertEquals( "FE1D2DTriElement", element.getWrappedFeature().getId() );
    assertEquals( 3, element.getEdges().size() );

    element = container.get( 2 );
    assertEquals( "ContinuityLine", element.getWrappedFeature().getId() );
    assertTrue( IFE1D2DContinuityLine.class.isAssignableFrom( element.getClass() ) );
*/
  }

//  public void testCreation( )
//  {
//    GMLWorkspace workspace = null;
//    try
//    {
//      workspace = GmlSerializer.createGMLWorkspace( TestWorkspaces.URL_EMPTY_GML, null );
//
//      Feature root = workspace.getRootFeature();
//      // gml:FeatureCollection
//
//      FE1D2DEdge edge = new FE1D2DEdge( root, TestWorkspaces.GML_PROP_FEATURE_MEMBER );
//      IFeatureWrapperCollection<IFE1D2DNode> nodes = edge.getNodes();
//
//      assertEquals( 0, nodes.size() );
//
//      IFeatureWrapperCollection<IFE1D2DElement> elements = edge.getContainers();
//
//      assertEquals( 0, elements.size() );
//
//      // test serial
//
//    }
//    catch( Throwable th )
//    {
//
//      fail( TestUtils.getStackTraceAsString( th ) );
//    }
//  }
}
