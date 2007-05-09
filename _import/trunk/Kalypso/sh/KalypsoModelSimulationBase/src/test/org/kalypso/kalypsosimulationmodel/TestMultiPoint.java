
package test.org.kalypso.kalypsosimulationmodel;


import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.kalypso.commons.xml.NS;
import org.kalypso.kalypsosimulationmodel.core.mpcoverage.MultiPoint;
import org.kalypso.kalypsosimulationmodel.schema.GmlImitationsConsts;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Test case for {@link MultiPoint}
 * 
 * @author Patrice Congo
 *
 */
public class TestMultiPoint extends TestCase
{
	
	
	public void testWorkspaceLoad()
	{
		
			
			GMLWorkspace workspace=null;
			
			try
			{
				workspace=
					GmlSerializer.createGMLWorkspace( 
									TestWorkspaces.URL_MULTIPOINT, 
									null );
			}
			catch(Throwable th)
			{
				fail(TestUtils.getStackTraceAsString(th));
			}
			
			Feature mpFeature=workspace.getRootFeature();
			
			MultiPoint mp= new MultiPoint(mpFeature);
			assertEquals(3,mp.size());
			
			assertEquals( 
					GeometryFactory.createGM_Point(0,0,null), 
					mp.get(0));
			assertEquals( 
					GeometryFactory.createGM_Point(0,1,null),//new GM_PointTestImpl(0,1,0), 
					mp.get(1));
			assertEquals( 
					GeometryFactory.createGM_Point(1,0,null),//new GM_PointTestImpl(1,0,0), 
					mp.get(2));
			
			
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
			QName featureMember= 
					new QName(NS.GML3,"featureMember");
			MultiPoint mp=
				new MultiPoint(
						root,
						featureMember);
			
			GM_Point p0=
				GeometryFactory.createGM_Point(0, 0, null);
			GM_Point p1=
				GeometryFactory.createGM_Point(1, 0,null);
			mp.add(p0);
			assertEquals(1, mp.size());
			mp.add(p1);
			assertEquals(2, mp.size());
			assertEquals(p0, mp.get(0));
			assertEquals(p1, mp.get(1));
			
			//find feature, points, point for direct comparison
			Feature mpFeature=
				(Feature)((List)root.getProperty(featureMember)).get(0);
			ArrayList points=
				(ArrayList)mpFeature.getProperty(
								GmlImitationsConsts.GML_PROP_POINT_MEMBER);
			assertEquals(mp, new MultiPoint(mpFeature));
			assertEquals(p0, points.get(0));
			assertEquals(p1, points.get(1));
			
			//delete
			mp.remove(0);
			assertEquals(1,mp.size());
			assertEquals(p1, mp.get(0));
			
			//clear
			mp.clear();
			assertEquals(0, mp.size());

			//insert delete 
			GM_Point p2=
				GeometryFactory.createGM_Point(2, 0,null);
			mp.add(p0);
			mp.add(p2);
			mp.add(1,p1);
			assertEquals(3, mp.size());
			assertEquals(p0, mp.get(0));
			assertEquals(p1, mp.get(1));
			assertEquals(p2, mp.get(2));
			mp.remove(1);
			assertEquals(2, mp.size());
			assertEquals(p0, mp.get(0));
			assertEquals(p2, mp.get(1));
		}
		catch(Throwable th)
		{
		
			fail(TestUtils.getStackTraceAsString(th));
		}
		
	}
}
