/**
 * 
 */
package test.org.kalypso.kalypsosimulationmodel;

import java.util.Arrays;

import org.deegree_impl.gml.GMLDocument_Impl;
import org.deegree_impl.gml.GMLFactory;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory;

import junit.framework.TestCase;

/**
 * @author Patrice Congo
 *
 */
public class TestPolynom1D extends TestCase
{
	public void testWorkspaceLoad()
	{
		try
		{
			IFeatureProviderFactory fpf;
			
			GMLWorkspace workspace=
				GmlSerializer.createGMLWorkspace( 
								TestWorkspaces.URL_POLYNOMS, null );
			
			System.out.println("Root="+workspace.getRootFeature().getProperty(
					KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_COEFFICIENTS));
			
			System.out.println("Root1="+
					Arrays.asList(workspace.getRootFeature().getProperties()));
			for(Object o:workspace.getRootFeature().getProperties())
			{
				System.out.println("GGGGGGG:"+o.getClass());
			}
		}
		catch(Throwable th)
		{
			th.printStackTrace();
			fail(th.getMessage());
		}
	}
}
