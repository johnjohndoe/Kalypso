package test.org.kalypso.kalypsomodel1d2d;

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2D_2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import test.org.kalypso.kalypsosimulationmodel.TestUtils;


import junit.framework.TestCase;

/**
 * Test the {@link RoughnessCls} implementaion by loading it from
 * a workspace and creating a new into a collection
 * 
 * @author Patrice Congo
 *
 */
public class TestFE1D2DQuadiElement extends TestCase
{
	public void testWorkspaceLoad()
	{
			
		GMLWorkspace workspace=null;
		
		try
		{
			workspace=
				GmlSerializer.createGMLWorkspace( 
								TestWorkspaces.URL_FE1D2D_QUADRI_ELE, 
								null );
		}
		catch(Throwable th)
		{
			fail(TestUtils.getStackTraceAsString(th));
		}
		Feature rFeature=workspace.getRootFeature();
		FE1D2D_2DElement quad= new FE1D2D_2DElement(rFeature);
		IFeatureWrapperCollection<IFE1D2DComplexElement> containers=
                        quad.getContainers();
        
        IFeatureWrapperCollection<IFE1D2DEdge> edges = quad.getEdges();
        
        assertEquals( 4, edges.size());
        IFE1D2DEdge e=edges.get( 0 );
        assertEquals( "edge1", e.getWrappedFeature().getId() );
        
        e=edges.get( 1 );
        assertEquals( "edge2", e.getWrappedFeature().getId() );
        
        e=edges.get( 2 );
        assertEquals( "edge3", e.getWrappedFeature().getId() );
        
        e=edges.get( 3 );
        assertEquals( "edge4", e.getWrappedFeature().getId() );
        
        
        
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
			
			FE1D2D_2DElement ele=
				new FE1D2D_2DElement(
						root,
						TestWorkspaces.GML_PROP_FEATURE_MEMBER,
                        Kalypso1D2DSchemaConstants.WB1D2D_MODEL1D2D_F_FE1D2DQuadriElement);
            
            IFeatureWrapperCollection<IFE1D2DEdge> edges=ele.getEdges();
            assertEquals( 0,  ele.getEdges().size());
            edges.addNew(  
                Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DEDGE);
            edges.addNew(  
                Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DEDGE);
            edges.addNew(  
                Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DEDGE);
            edges.addNew(  
                Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DEDGE);
            
            
            assertEquals( 4,  ele.getEdges().size());
            //test serial
            
        }
        catch(Throwable th)
		{
		
			fail(TestUtils.getStackTraceAsString(th));
		}		
	}
}
