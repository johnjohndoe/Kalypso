package test.org.kalypso.kalypsosimulationmodel;

import java.lang.reflect.Array;
import java.util.Arrays;

import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessPolygon;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;

import junit.framework.TestCase;

/**
 * Tests the {@link RoughnessPolygon} class by loading a gml workspace
 * from file and creating a {@link RoughnessPolygon} into a workspace as
 * collection member
 * 
 * @author Patrice Congo
 *
 */
public class TestRoughnessPolygon extends TestCase
{

	
	public void testWorkspaceLoad()
	{
			
		GMLWorkspace workspace=null;
		
		try
		{
			workspace=
				GmlSerializer.createGMLWorkspace( 
								TestWorkspaces.URL_ROUGHNESS_POLYGON, 
								null );
		}
		catch(Throwable th)
		{
			fail(TestUtils.getStackTraceAsString(th));
		}
		Feature rFeature=workspace.getRootFeature();
		RoughnessPolygon rp= 
			new RoughnessPolygon(rFeature);
		assertEquals(
				"htpp://wwww.tuhh.de/wb/roughness_db/grass", 
				rp.getRoughnessID());
		GM_Polygon pol=rp.getPolygon();
		GM_Position[] positions=pol.getExteriorRing();
		double[][] posArray={{0,0},{0,1},{1,1},{1,0},{0,0}};
		
		int i=0;
		for(GM_Position pos:positions)
		{
			assertTrue(Arrays.equals(posArray[i], pos.getAsArray()));
			i++;
		}
	}
}
