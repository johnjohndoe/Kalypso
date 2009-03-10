/**
 * 
 */
package test.org.kalypso.kalypsosimulationmodel;

import java.util.List;

import junit.framework.TestCase;

import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author congo
 */
public class TestRoughnessClsCollection extends TestCase
{
  public void testWorkspaceLoad( )
  {

    GMLWorkspace workspace = null;

    try
    {
      workspace = GmlSerializer.createGMLWorkspace( TestWorkspaces.URL_ROUGHNESS_CLS_COLLECTION, null );
    }
    catch( Throwable th )
    {
      fail( TestUtils.getStackTraceAsString( th ) );
    }

    Feature root = workspace.getRootFeature();
    RoughnessClsCollection rcc = new RoughnessClsCollection( root );
    assertEquals( "ColName1", rcc.getName()); //$NON-NLS-1$
    // assertEquals("r2", rcc.getRoughnessByURI("uri_r2").getName());
    // assertEquals("uri_r2", rcc.getRoughnessByURI("uri_r2").getURI());
    List<IRoughnessCls> rList = rcc.selectRoughnessByName( "Klass 2" ); //$NON-NLS-1$
    assertEquals( 1, rList.size() );
    // System.out.println("LIST="+rList);
    // assertEquals("uri_r2", rList.get(0).getURI());

  }
}
