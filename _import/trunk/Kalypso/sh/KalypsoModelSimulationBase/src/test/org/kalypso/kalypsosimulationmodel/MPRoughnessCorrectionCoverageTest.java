/**
 * 
 */
package test.org.kalypso.kalypsosimulationmodel;

import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.kalypsosimulationmodel.util.math.Polynomial1D;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import junit.framework.TestCase;

/**
 * 
 * @author Patrice Congo
 */
public class MPRoughnessCorrectionCoverageTest extends TestCase
{

	public void testMPCovLoad()
	{
		
			//IFeatureProviderFactory fpf;
			
			GMLWorkspace workspace=null;
			
			try
			{
				workspace=
					GmlSerializer.createGMLWorkspace( 
									TestWorkspaces.URL_MPCOV_ROUGHNESS_CORRECTION, 
									null );
			}
			catch(Throwable th)
			{
				fail(TestUtils.getStackTraceAsString(th));
			}
			Feature mpCovFeature=workspace.getRootFeature();
			System.out.println("coveFeature:"+mpCovFeature);
			
//			assertEquals(
//					pol1dFeature.getFeatureType().getQName(), 
//					KalypsoModelSimulationBaseConsts.SIM_BASE_PLYNOMIAL1D);
//			Polynomial1D pol1d= new Polynomial1D(pol1dFeature);
//			
//			assertEquals(EXPECTED_POL1D,pol1d);
			
	}
}
