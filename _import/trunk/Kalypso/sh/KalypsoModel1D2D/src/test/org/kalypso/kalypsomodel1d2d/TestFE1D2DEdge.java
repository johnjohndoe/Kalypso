/**
 * 
 */
package test.org.kalypso.kalypsomodel1d2d;

import junit.framework.TestCase;

import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DContinuityLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IPolyElement;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import test.org.kalypso.kalypsosimulationmodel.TestUtils;

/**
 * Test the {@link RoughnessCls} implementaion by loading it from
 * a workspace and creating a new into a collection
 * 
 * @author Patrice Congo
 *
 */
public class TestFE1D2DEdge extends TestCase
{
	public void testWorkspaceLoad()
	{
			
		GMLWorkspace workspace=null;
		
		try
		{
			workspace=
				GmlSerializer.createGMLWorkspace( 
								TestWorkspaces.URL_FE1D2DEDGE, 
								null );
		}
		catch(Throwable th)
		{
			fail(TestUtils.getStackTraceAsString(th));
		}
		Feature rFeature=workspace.getRootFeature();
		FE1D2DEdge edge= new FE1D2DEdge(rFeature);
        IFeatureWrapperCollection<IFE1D2DElement> container = edge.getContainers();
        assertEquals( 3, container.size());
        
        IFE1D2DElement element=container.get( 0 );
        final IPolyElement quadriElement = (IPolyElement) element.getWrappedFeature().getAdapter( IPolyElement.class );
        assertEquals( "QuadriElement", element.getWrappedFeature().getId() );
        assertEquals( 4, quadriElement.getEdges().size() );
        
        element=container.get( 1 );
        final IPolyElement triElement = (IPolyElement) element.getWrappedFeature().getAdapter( IPolyElement.class );
        assertEquals( "FE1D2DTriElement", element.getWrappedFeature().getId() );
        assertEquals( 3, triElement.getEdges().size() );
        
        element=container.get( 2 );
        assertEquals( "ContinuityLine", element.getWrappedFeature().getId() );       
        assertTrue( 
            IFE1D2DContinuityLine.class.isAssignableFrom( element.getClass() )  );
        
	}
	
	public void testCreation()
	{
		GMLWorkspace workspace= null;
		try
		{
			workspace=
				GmlSerializer.createGMLWorkspace( 
					TestWorkspaces.URL_EMPTY_GML, null );
			
			Feature root=
				workspace.getRootFeature();	
			//gml:FeatureCollection 
			
			FE1D2DEdge edge=
				new FE1D2DEdge(
						root,
						TestWorkspaces.GML_PROP_FEATURE_MEMBER);
            IFeatureWrapperCollection<IFE1D2DNode> nodes = edge.getNodes();
            
            assertEquals( 0, nodes.size() );
            
            IFeatureWrapperCollection<IFE1D2DElement> elements =
                                                edge.getContainers();
            
            assertEquals( 0,  elements.size());
            
            //test serial
            
        }
        catch(Throwable th)
		{
		
			fail(TestUtils.getStackTraceAsString(th));
		}		
	}
}
