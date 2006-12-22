/**
 * 
 */
package test.org.kalypso.kalypsosimulationmodel;

import java.util.Arrays;

import ogc31.www.opengis.net.gml.PointType;
import ogc31.www.opengis.net.gml.RangeSetType;
import ogc31.www.opengis.net.gml.ValueArrayType;

import org.deegree.model.feature.FeatureCollection;
import org.kalypso.kalypsosimulationmodel.schema.GmlImitationsConsts;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.kalypsosimulationmodel.util.math.Polynomial1D;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.cv.RangeSetTypeHandler;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

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
			System.out.println(
					"coveFeature:"+
					Arrays.asList(mpCovFeature.getProperties()));
			
			Feature rst=
				(Feature)mpCovFeature.getProperty(
									GmlImitationsConsts.WBGML_PROP_RANGESET);
			FeatureCollection col;
			Feature fMember1=
				(Feature)rst.getProperty(GmlImitationsConsts.GML_PROP_FEATURE_MEMBER);
			Feature fMember2=
				(Feature)rst.getProperty(GmlImitationsConsts.GML_PROP_FEATURE_MEMBER);
			System.out.println(
					"cur="+
					fMember1+" "+fMember2+" "+(fMember1==fMember2));
			
			
			
			
//			assertEquals(
//					pol1dFeature.getFeatureType().getQName(), 
//					KalypsoModelSimulationBaseConsts.SIM_BASE_PLYNOMIAL1D);
//			Polynomial1D pol1d= new Polynomial1D(pol1dFeature);
//			
//			assertEquals(EXPECTED_POL1D,pol1d);
			
	}
}
