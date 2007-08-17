/**
 * 
 */
package test.org.kalypso.kalypsosimulationmodel;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.kalypso.commons.xml.NS;
import org.kalypso.kalypsosimulationmodel.core.mpcoverage.FeatureRangeSet;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;

/**
 * Test case for the feature range set.
 * Test loading a workspace, creating an Feature range set as a child
 * element  for a collection and and adding  properties
 *  
 * @author Patrice Congo
 *
 */
public class TestFeatureRangeSet extends TestCase
{

	public void testWorkspaceLoad()
	{
		
			
			GMLWorkspace workspace=null;
			
			try
			{
				workspace=
					GmlSerializer.createGMLWorkspace( 
									TestWorkspaces.URL_FEATURERANGESET, 
									null );
			}
			catch(Throwable th)
			{
				fail(TestUtils.getStackTraceAsString(th));
			}
			
			Feature rsFeature = workspace.getRootFeature();
			
			FeatureRangeSet<IPolynomial1D> frs=
				new FeatureRangeSet<IPolynomial1D>(rsFeature,IPolynomial1D.class);
			
			assertEquals(2, frs.size());
			assertEquals("pol1", frs.get(0).getWrappedFeature().getId()); //$NON-NLS-1$
			assertEquals("pol2", frs.get(1).getWrappedFeature().getId()); //$NON-NLS-1$
			IPolynomial1D pol1=frs.get(0);
			
			//remove
			frs.remove(0);
			assertEquals(1, frs.size());
			assertEquals("pol2", frs.get(0).getWrappedFeature().getId()); //$NON-NLS-1$
			
			//add last
			frs.add(pol1);
			assertEquals(2, frs.size());
			assertEquals("pol2", frs.get(0).getWrappedFeature().getId()); //$NON-NLS-1$
			assertEquals("pol1", frs.get(1).getWrappedFeature().getId()); //$NON-NLS-1$
			
			//System.out.println(frs.get(0));
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
					new QName(NS.GML3,"featureMember"); //$NON-NLS-1$
			FeatureRangeSet<IPolynomial1D> frs=
				new FeatureRangeSet<IPolynomial1D>(
						root,
						featureMember,
						IPolynomial1D.class);
			FeatureList frsFList=
				(FeatureList)root.getProperty(featureMember);
			FeatureRangeSet<IPolynomial1D> frsPol1d=
					new FeatureRangeSet<IPolynomial1D>(
												(Feature)frsFList.get(0),
												IPolynomial1D.class);
			assertEquals(0, frs.size());
			assertEquals(frsPol1d, frs);
			
            // this test could never work, because the name of the relation
            // of the example file is not 'polynomial1d' but 'featureMember'
//			Polynomial1D pol1= new Polynomial1D(workspace);
//			pol1.setCoefficients(new double[]{0,1});
//			frs.add(pol1);
//			assertEquals(1, frs.size());
//			assertEquals(pol1, frs.get(0));
		}
		catch(Throwable th)
		{
		
			fail(TestUtils.getStackTraceAsString(th));
		}
		
	}
    
//     public IPolynomial1D polynomial1DFromWorkspace(GMLWorkspace workWorkspace)
//     {
//       IFeatureType featureType=
//       workWorkspace.getGMLSchema().getFeatureType(
//       IPolynomial1D.QNAME);
//              
//       final IFeatureType parentFT = workWorkspace.getRootFeature().getFeatureType();
//              
//       // TODO: @Patrice: this is probably not always the case, so better provide parent instead of workspace as parameter
//       final IRelationType parentRelation = (IRelationType) parentFT.getProperty(
//       KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_POLYNOMIAL1D );
//              
//       this.polFeature=
//       FeatureFactory.createFeature(
//       workWorkspace.getRootFeature(),parentRelation,
//       "Polynom1d"+System.currentTimeMillis(),//TODO better ids , random?
//       featureType,
//       true);
//     }
}
