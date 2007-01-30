package test.org.kalypso.kalypsomodel1d2d;

import java.util.List;

import junit.framework.TestCase;

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2D_2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import test.org.kalypso.kalypsosimulationmodel.TestUtils;

/**
 * Test the {@link RoughnessCls} implementaion by loading it from a workspace and creating a new into a collection
 * 
 * @author Patrice Congo
 */
public class TestFE1D2DPolyElement extends TestCase
{
  public void testWorkspaceLoad( )
  {

    GMLWorkspace workspace = null;

    try
    {
      workspace = GmlSerializer.createGMLWorkspace( TestWorkspaces.URL_FE1D2D_QUADRI_ELE, null );
    }
    catch( Throwable th )
    {
      fail( TestUtils.getStackTraceAsString( th ) );
    }
    Feature rFeature = workspace.getRootFeature();
    FE1D2D_2DElement quad = new FE1D2D_2DElement( rFeature );

    IFeatureWrapperCollection<IFE1D2DEdge> edges = quad.getEdges();

    assertEquals( 4, edges.size() );
    IFE1D2DEdge e = edges.get( 0 );
    assertEquals( "edge1", e.getWrappedFeature().getId() );
    
    
    e = edges.get( 1 );
    assertEquals( "edge2", e.getWrappedFeature().getId() );

    e = edges.get( 2 );
    assertEquals( "edge3", e.getWrappedFeature().getId() );

    e = edges.get( 3 );
    assertEquals( "edge4", e.getWrappedFeature().getId() );
    
//  test get nodes
    List<IFE1D2DNode> nodes=quad.getNodes();
    assertEquals( 
        "node0", 
        nodes.get( 0 ).getWrappedFeature().getId() );
    assertEquals( 
        "node1", 
        nodes.get( 1 ).getWrappedFeature().getId() );
    assertEquals( 
        "node0", 
        nodes.get( 2 ).getWrappedFeature().getId() );
    assertEquals( 
        "node1", 
        nodes.get( 3 ).getWrappedFeature().getId() );
    assertEquals( 
        "node0", 
        nodes.get( 4 ).getWrappedFeature().getId() );
  }

  public void testCreationQuad( )
  {
    GMLWorkspace workspace = null;
    try
    {
      workspace = GmlSerializer.createGMLWorkspace( TestWorkspaces.URL_EMPTY_GML, null );

      Feature root = workspace.getRootFeature();
      // gml:FeatureCollection

      FE1D2D_2DElement ele = new FE1D2D_2DElement( root, TestWorkspaces.GML_PROP_FEATURE_MEMBER, Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT );

      IFeatureWrapperCollection<IFE1D2DEdge> edges = ele.getEdges();
      assertEquals( 0, ele.getEdges().size() );
      edges.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE );
      edges.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE );
      edges.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE );
      edges.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE );

      assertEquals( 4, ele.getEdges().size() );
      // test serial

    }
    catch( Throwable th )
    {

      fail( TestUtils.getStackTraceAsString( th ) );
    }
  }

  public void testCreationTri( )
  {
    GMLWorkspace workspace = null;
    try
    {
      workspace = GmlSerializer.createGMLWorkspace( TestWorkspaces.URL_EMPTY_GML, null );

      Feature root = workspace.getRootFeature();
      // gml:FeatureCollection

      FE1D2D_2DElement ele = new FE1D2D_2DElement( root, TestWorkspaces.GML_PROP_FEATURE_MEMBER, Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT );

      IFeatureWrapperCollection<IFE1D2DEdge> edges = ele.getEdges();
      assertEquals( 0, ele.getEdges().size() );
      edges.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE );
      edges.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE );
      edges.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE );

      // TODO implements and test size check

      assertEquals( 3, ele.getEdges().size() );
      // test serial

    }
    catch( Throwable th )
    {

      fail( TestUtils.getStackTraceAsString( th ) );
    }
  }

}
