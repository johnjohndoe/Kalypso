package test.org.kalypso.kalypsosimulationmodel;

import junit.framework.TestCase;

import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelRoughnessConsts;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;

/**
 * Test the {@link FeatureWrapperCollection} class using a feature 
 * collection of wbr:RoughnessCollectionCls
 * 
 * @author Patrice Congo
 *
 */
public class TestFeatureWrapperCollection extends TestCase
{

	public void testWorkspaceLoad()
	{
			
		GMLWorkspace workspace=null;
		
		try
		{
			workspace=
				GmlSerializer.createGMLWorkspace( 
								TestWorkspaces.URL_COL_ROUGHNESS_CLS_COR, 
								null );
		}
		catch(Throwable th)
		{
			fail(TestUtils.getStackTraceAsString(th));
		}
		Feature colF=workspace.getRootFeature();
		FeatureWrapperCollection<IRoughnessClsCorrection> fwc = 
				new FeatureWrapperCollection<IRoughnessClsCorrection>(
									colF,
									IRoughnessClsCorrection.class,
									TestWorkspaces.GML_PROP_FEATURE_MEMBER);
		assertEquals(2, fwc.size());
		String uris[]={"cor1","cor2"}; //$NON-NLS-1$ //$NON-NLS-2$
		assertEquals(uris[0],fwc.get(0).getURI());
		assertEquals(uris[1],fwc.get(1).getURI());
		
		int i=0;
		for(IRoughnessClsCorrection rcc:fwc)
		{
			assertEquals(uris[i],rcc.getURI());
			i++;
		}
		
		//remove
		IRoughnessClsCorrection rcc=fwc.remove(0);
		assertEquals(uris[1],fwc.get(0).getURI());
		
		//add at
		fwc.add(1, rcc);
		assertEquals(uris[0],fwc.get(1).getURI());
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
			FeatureWrapperCollection<IRoughnessClsCorrection> fwc=
				new FeatureWrapperCollection<IRoughnessClsCorrection>(
						root,
						IRoughnessClsCorrection.class,
						TestWorkspaces.GML_PROP_FEATURE_MEMBER);
			IRoughnessClsCorrection added=
				fwc.addNew(KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS_CORRECTION);
			added.setAxAyCor(0.12547881);
			assertEquals(added, fwc.get(0));
		}
		catch(Throwable th)
		{
		
			fail(TestUtils.getStackTraceAsString(th));
		}		
	}
}
