/**
 * 
 */
package test.org.kalypso.kalypsosimulationmodel;

import java.lang.reflect.Array;
import java.util.Arrays;

import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.kalypsosimulationmodel.util.math.Polynomial1D;
import org.kalypso.kalypsosimulationmodel.util.math.Polynomial2D;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import junit.framework.TestCase;

/**
 * @author congo
 *
 */
public class TestPolynomial2D extends TestCase
{
	
	public void testLoadingFromGML()
	{
		GMLWorkspace workspace=null;
		
		try
		{
			workspace=
				GmlSerializer.createGMLWorkspace( 
								TestWorkspaces.URL_POLYNOMIAL2D, 
								null );
		}
		catch(Throwable th)
		{
			fail(TestUtils.getStackTraceAsString(th));
		}
		
		Feature pol2dFeature=workspace.getRootFeature();
		assertEquals(
				KalypsoModelSimulationBaseConsts.SIM_BASE_F_POLYNOMIAL2D,
				pol2dFeature.getFeatureType().getQName()
				);
		Polynomial2D pol2d= new Polynomial2D(pol2dFeature);
		//test orders
		try
		{
			assertEquals(3,pol2d.getDegreeX());
			assertEquals(1,pol2d.getDegreeY());
		}
		catch(Throwable th)
		{
			fail(TestUtils.getStackTraceAsString(th));
		}
		//test coefs
		try
		{
			double[] expectedArray={0,1,2,3,4,5,6,7};
			double[] actualArray=pol2d.getCoefficients();
			assertTrue(
					"Ceofficients unexpected",
					Arrays.equals(expectedArray, actualArray));
		}
		catch(Throwable th)
		{
			System.out.println(Arrays.asList(pol2dFeature.getProperties()));
			fail(TestUtils.getStackTraceAsString(th));
		}
	}
}
