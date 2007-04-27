package test.org.kalypso.kalypsosimulationmodel;

import java.io.OutputStreamWriter;

import junit.framework.TestCase;

import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessPolygon;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfaceInterpolation;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

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

	public void testOutput()
	{
		GMLWorkspace workspace=null;		
		try
		{
			workspace=
				GmlSerializer.createGMLWorkspace( 
								TestWorkspaces.URL_EMPTY_GML, 
								null );
			Feature rFeature=workspace.getRootFeature();
			
			Feature rp=
				FeatureHelper.addFeature(
					rFeature, 
					TestWorkspaces.GML_PROP_FEATURE_MEMBER,
					KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON);
			
			//gm pos
			GM_Position exteriorRing[]= new GM_Position[5];
			
			double[][] posArray={{0,0},{0,1},{1,1},{1,0},{0,0}};
			for(int i=0;i<5;i++)
			{
				exteriorRing[i]=
					GeometryFactory.createGM_Position(posArray[i]);
			}
			
			final CS_CoordinateSystem CS_GAUSS_KRUEGER=
				ConvenienceCSFactory.getInstance().getOGCCSByName(
									TestWorkspaces.CS_KEY_GAUSS_KRUEGER);
			
//			GM_SurfacePatch pol1=
//				GeometryFactory.createGM_SurfacePatch(
//								exteriorRing, 
//								new  GM_Position[0][0], 
//								(GM_SurfaceInterpolation)null, 
//								CS_GAUSS_KRUEGER);
//				GM_Surface surface=
//					GeometryFactory.createGM_Surface(pol1);
			
			GM_Surface surface=
				GeometryFactory.createGM_Surface(
						exteriorRing, 
						new  GM_Position[0][0], 
						(GM_SurfaceInterpolation)null, 
						CS_GAUSS_KRUEGER);
			
			
//			System.out.println("RP_PROPS=="+pol1.getClass()+
//					"\n "+pol1);
//			System.out.println("RP_PROPS=="+Arrays.asList(rp.getProperties()));
			
			rp.setProperty(
					KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_POLYGON, 
					surface//pol1//surface
					);
			
			rp.setProperty(
					KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_STYLE, 
					"htpp://wwww.tuhh.de/wb/roughness_db/grass");
			
//			FeatureHelper.addProperty(
//					rp,
//					KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_POLYGON, 
//					pol1);
			System.out.println("=====================================");
			OutputStreamWriter writer= new OutputStreamWriter(System.out);
			GmlSerializer.serializeWorkspace(writer, workspace);
			
		}
		catch(Throwable th)
		{
			fail(TestUtils.getStackTraceAsString(th));
		}
		
	}
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
		FeatureList list=
			(FeatureList)rFeature.getProperty(TestWorkspaces.GML_PROP_FEATURE_MEMBER);
		RoughnessPolygon rp= 
			new RoughnessPolygon((Feature)list.get(0));
		assertEquals(
				"Sand und Schilf", 
				rp.getRoughnessStyle());
//		GM_Polygon pol=rp.getPolygon();
//		GM_Position[] positions=pol.getExteriorRing();
//		double[][] posArray={{0,0},{0,1},{1,1},{1,0},{0,0}};
//		
//		int i=0;
//		for(GM_Position pos:positions)
//		{
//			assertTrue(Arrays.equals(posArray[i], pos.getAsArray()));
//			i++;
//		}
		
		
	}
}
