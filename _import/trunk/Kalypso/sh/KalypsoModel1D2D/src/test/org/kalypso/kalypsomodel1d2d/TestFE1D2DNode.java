/**
 * 
 */
package test.org.kalypso.kalypsomodel1d2d;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

import test.org.kalypso.kalypsosimulationmodel.TestUtils;

/**
 * Test the {@link RoughnessCls} implementaion by loading it from
 * a workspace and creating a new into a collection
 * 
 * @author Patrice Congo
 *
 */
public class TestFE1D2DNode extends TestCase
{
	public void testWorkspaceLoad()
	{
			
		GMLWorkspace workspace=null;
		
		try
		{
			workspace=
				GmlSerializer.createGMLWorkspace( 
								TestWorkspaces.URL_FE1D2DNODE, 
								null );
		}
		catch(Throwable th)
		{
			fail(TestUtils.getStackTraceAsString(th));
		}
		Feature rFeature=workspace.getRootFeature();
		FE1D2DNode node= new FE1D2DNode(rFeature);
        IFeatureWrapperCollection<IFE1D2DEdge> container = node.getContainers();
        assertEquals( 2, container.size());
        assertEquals( "edge1", container.get( 0 ).getWrappedFeature().getId() );
        assertEquals( "edge2", container.get( 1 ).getWrappedFeature().getId() );
        assertEquals( 0.0010, node.getPoint().getY());
        assertEquals( 0.0020, node.getPoint().getX());
        
        QName propQName = new QName(UrlCatalog1D2D.MODEL_1D2D_NS,"hasElevation");
        System.out.println("hasElevation="+rFeature.getProperty( propQName  ));
	}
	
	public void testCreation()
	{
		GMLWorkspace workspace= null;
		try
		{
			workspace=
				GmlSerializer.createGMLWorkspace( 
					TestWorkspaces.URL_EMPTY_GML, null );;
			
			Feature root=
				workspace.getRootFeature();	
			//gml:FeatureCollection 
			
			FE1D2DNode node=
				new FE1D2DNode(
						root,
						TestWorkspaces.GML_PROP_FEATURE_MEMBER);
            CS_CoordinateSystem cs= 
              ConvenienceCSFactory.getInstance().getOGCCSByName( 
                                  TestWorkspaces.CS_KEY_GAUSS_KRUEGER );
			GM_Point p1= 
                  GeometryFactory.createGM_Point(0.1, 0.4, cs );
            node.setPoint( p1 );
            assertEquals( p1, node.getPoint() );
            IFeatureWrapperCollection<IFE1D2DEdge > edges=
                            node.getContainers();
            assertNotNull( edges );
            assertEquals( 0, edges.size() );
            IFE1D2DEdge edge=
              edges.addNew( 
                Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE );
            assertNotNull( edge );
            assertEquals( 1, edges.size() );
            
            final String NODE_ID= "_node_id_1_";
            FE1D2DNode created=
                new FE1D2DNode(root,TestWorkspaces.GML_PROP_FEATURE_MEMBER,NODE_ID);
            created.setPoint( GeometryFactory.createGM_Point(0.11, 0.44, cs ) );
            Feature getNodeFeature=workspace.getFeature( NODE_ID );
            assertNotNull( getNodeFeature );
            
            //test serial
            
        }
        catch(Throwable th)
		{
		
			fail(TestUtils.getStackTraceAsString(th));
		}		
	}
}
