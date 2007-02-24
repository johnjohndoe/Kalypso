/**
 * 
 */
package test.org.kalypso.kalypsosimulationmodel;



import junit.framework.TestCase;

import org.kalypso.kalypsosimulationmodel.core.mpcoverage.MultiPointCoverage;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Test the  class with a coverage over
 * polynomial 1d elements
 * 
 * @author Patrice Congo
 */
public class MSurfaceCoverageTest extends TestCase
{
	
	public void testMPCovLoad()
	{
		GMLWorkspace workspace=null;
		
		try
		{
			workspace=
				GmlSerializer.createGMLWorkspace( 
								TestWorkspaces.URL_MPCOV_ROUGHNESS_CORRECTION, 
								null );
			Feature root=workspace.getRootFeature();
			MultiPointCoverage<IPolynomial1D> mpcPol1d=
				new MultiPointCoverage<IPolynomial1D>(
										root,
										IPolynomial1D.class);
			//point00
			GM_Point point=
				GeometryFactory.createGM_Point(0, 0, null);
			IPolynomial1D pol=mpcPol1d.getRangeValue(point);
			assertEquals("pol1", pol.getWrappedFeature().getId());
			
			//point 01
			point=
				GeometryFactory.createGM_Point(0, 1, null);
			pol=mpcPol1d.getRangeValue(point);
			assertEquals("pol2", pol.getWrappedFeature().getId());
			
			//point10
			point=
				GeometryFactory.createGM_Point(1, 0, null);
			pol=mpcPol1d.getRangeValue(point);
			assertEquals("pol3", pol.getWrappedFeature().getId());
		}
		catch(Throwable th)
		{
			fail(TestUtils.getStackTraceAsString(th));
		}
	}

//	public void testMPCovLoad()
//	{
//		
//			//IFeatureProviderFactory fpf;
//			
//			GMLWorkspace workspace=null;
//			
//			try
//			{
//				workspace=
//					GmlSerializer.createGMLWorkspace( 
//									TestWorkspaces.URL_MPCOV_ROUGHNESS_CORRECTION, 
//									null );
//			}
//			catch(Throwable th)
//			{
//				fail(TestUtils.getStackTraceAsString(th));
//			}
//			Feature mpCovFeature=workspace.getRootFeature();
//			System.out.println(
//					"coveFeature:"+
//					Arrays.asList(mpCovFeature.getProperties()));
//			
//			Feature rst=
//				(Feature)mpCovFeature.getProperty(
//									GmlImitationsConsts.WBGML_PROP_RANGESET);
//			FeatureCollection col;
//			FeatureList fMember1=
//				(FeatureList)rst.getProperty(GmlImitationsConsts.GML_PROP_FEATURE_MEMBER);
//			for(int i=fMember1.size()-1;i>=0;i--)
//			{
//				System.out.println(
//						"curMemeber["+i+"]"+
//						fMember1.get(i));
//			}
//			
//			
//			Feature multiPointDomain=
//				(Feature)mpCovFeature.getProperty(
//						GmlImitationsConsts.WBGML_PROP_MULTIPOINT_DOMAIN);
//			System.out.println("multiPointDomain"+multiPointDomain);
//			ArrayList<Object> pointMember=
//				(ArrayList<Object>)multiPointDomain.getProperty(
//						GmlImitationsConsts.GML_PROP_POINT_MEMBER);
//			System.out.println("PointMember="+pointMember.getClass());
//			OutputStreamWriter osw= new OutputStreamWriter(System.out);
//			try
//			{
//				pointMember.remove(0);
//				GmlSerializer.serializeWorkspace(
//					osw, multiPointDomain.getWorkspace());
//			}
//			catch(Throwable th)
//			{
//				th.printStackTrace();
//				fail();
//			}
////			assertEquals(
////					pol1dFeature.getFeatureType().getQName(), 
////					KalypsoModelSimulationBaseConsts.SIM_BASE_PLYNOMIAL1D);
////			Polynomial1D pol1d= new Polynomial1D(pol1dFeature);
////			
////			assertEquals(EXPECTED_POL1D,pol1d);
//			
//	}
}
