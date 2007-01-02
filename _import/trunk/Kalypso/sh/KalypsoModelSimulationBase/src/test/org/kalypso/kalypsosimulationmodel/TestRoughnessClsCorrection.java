package test.org.kalypso.kalypsosimulationmodel;

import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCorrection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import junit.framework.TestCase;

/**
 * Test the {@link RoughnessClsCorrection} class by loading 
 * from a gml workspace and creating a new one into a workspace
 * 
 * @author Patrice Congo
 *
 */
public class TestRoughnessClsCorrection extends TestCase
{
	public void testWorkspaceLoad()
	{
			
		GMLWorkspace workspace=null;
		
		try
		{
			workspace=
				GmlSerializer.createGMLWorkspace( 
								TestWorkspaces.URL_ROUGHNESS_CLS_COR, 
								null );
		}
		catch(Throwable th)
		{
			fail(TestUtils.getStackTraceAsString(th));
		}
		Feature rcFeature=workspace.getRootFeature();
		RoughnessClsCorrection rc= 
			new RoughnessClsCorrection(rcFeature);
		/*
		 * 	<wbr:ks>0.1</wbr:ks>
			<wbr:axay>0.2</wbr:axay>
			<wbr:dp>0.3</wbr:dp>
			<wbr:eddy>0.4</wbr:eddy>
			<wbr:marsh>0.5</wbr:marsh>
		 */
		assertEquals(
				0.1, 
				rc.getKsCor());
		
		assertEquals(
				0.2, 
				rc.getAxAyCor());
		
		assertEquals(
				0.3, 
				rc.getDpCor());
		
		assertEquals(
				0.4, 
				rc.getEddyCor());
		assertEquals(
				0.5, 
				rc.getMarshCor());

		assertEquals(
				"htpp://wwww.tuhh.de/wb/roughness_db/grass", 
				rc.getURI());
		assertNull(
				"", 
				rc.getDescription());
		
		//setter
		rc.setAxAyCor(0.11);
		assertEquals(
				0.11, 
				rc.getAxAyCor());
		
		rc.setDpCor(0.22);
		assertEquals(
				0.22, 
				rc.getDpCor());
		
		rc.setEddyCor(0.33);
		assertEquals(
				0.33, 
				rc.getEddyCor());
		
		rc.setKsCor(0.44);
		assertEquals(
				0.44, 
				rc.getKsCor());
		
		rc.setMarshCor(0.55);
		assertEquals(
				0.55, 
				rc.getMarshCor());
		
		final String URI="_URI_";
		rc.setURI(URI);
		assertEquals(URI, rc.getURI());
	}
}
