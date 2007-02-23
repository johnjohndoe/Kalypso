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

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import junit.framework.TestCase;

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DDiscretisationModel;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

import test.org.kalypso.kalypsosimulationmodel.TestUtils;

/**
 * Tests the Discretisation model 
 * 
 * @author Patrice Congo
 *
 */
public class TestDiscretisationModel1D2D extends TestCase
{

  public TestDiscretisationModel1D2D( )
  {
    
  }

  public TestDiscretisationModel1D2D( String name )
  {
    super( name );
  }

  private static final IFEDiscretisationModel1d2d createTestDiscretisationModel()
  {
    File tmpFile=null;
    GMLWorkspace workspace =null;
    InputStream aggerStream=null;
    
    try
    {
      String tempFileName = 
        "test_discretisation_model"+System.currentTimeMillis();
      tmpFile=File.createTempFile( tempFileName, "gml" );
    }
    catch( IOException e )
    {
      fail( TestUtils.getStackTraceAsString( e ) );
    }
    
    try
    {
      workspace =
          FeatureFactory.createGMLWorkspace(
              Kalypso1D2DSchemaConstants.WB1D2D_F_DiscretisationModel, 
              tmpFile.toURL(), 
              GmlSerializer.DEFAULT_FACTORY);
      
    }
    catch( Exception e )
    {
      fail( TestUtils.getStackTraceAsString( e ));
    }
    
    
    
    FE1D2DDiscretisationModel targetModel=
        new FE1D2DDiscretisationModel(workspace.getRootFeature());
    return targetModel;
  }
  
  public void testFindNode()
  {
    IFEDiscretisationModel1d2d model = createTestDiscretisationModel();
    CS_CoordinateSystem GAUSS_KRUEGER = TestWorkspaces.getGaussKrueger();
    boolean alreadyExists[]= new boolean[1];
    final int MAX_I=20;
    final int MAX_J=20;
    final IFE1D2DNode<IFE1D2DEdge>[][]  nodes = 
                      (IFE1D2DNode<IFE1D2DEdge>[][])new IFE1D2DNode[MAX_I][MAX_J];
    
    //fill the modell
    for(int i=0;i<MAX_I;i++)
    {
      int i10=10*i;
      for(int j=0;j<MAX_J;j++)
      {
        GM_Point nodePoint =
          GeometryFactory.createGM_Point( 
                        i10, j*10, GAUSS_KRUEGER );
        nodes[i][j]=model.createNode( nodePoint,5, alreadyExists );
        assertFalse( 
            "Points not cannot be in model but already exists",
            alreadyExists[0] );
        
      }
    }
    
    //try finding the nodes
    for(int i=0;i<MAX_I;i++)
    {
      int i10=10*i;
      for(int j=0;j<MAX_J;j++)
      {
        GM_Point nodePoint =
          GeometryFactory.createGM_Point( 
                        i10, j*10, GAUSS_KRUEGER );
        IFE1D2DNode foundNode = model.findNode( nodePoint, 5 );
        assertNotNull( 
            "Node should be found ans not null"+model.getNodes().size(), 
            foundNode );
        assertEquals( 
            nodes[i][j].getGmlID(), 
            foundNode.getGmlID() );
        
      }
    }
  }
}
