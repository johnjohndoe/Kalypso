/**
 * 
 */
package test.org.kalypso.kalypsosimulationmodel;

import junit.framework.TestCase;

import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessCls;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Test the {@link RoughnessCls} implementaion by loading it from a workspace and creating a new into a collection
 * 
 * @author Patrice Congo
 * 
 */
public class TestRougnessCls extends TestCase
{
  public void testWorkspaceLoad( ) throws Exception
  {
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( TestWorkspaces.URL_ROUGHNESS_CLS, null );

    final Feature rFeature = workspace.getRootFeature();
    final RoughnessCls roughnessCls = (RoughnessCls) rFeature;
    /*
     * <wbr:ks>0.1</wbr:ks> <wbr:axay>0.2</wbr:axay> <wbr:dp>0.3</wbr:dp> <wbr:eddy>0.4</wbr:eddy>
     * <wbr:marsh>0.5</wbr:marsh>
     */
    assertEquals( 0.1, roughnessCls.getKs() );

    assertEquals( 0.2, roughnessCls.getAxAy() );

    assertEquals( 0.3, roughnessCls.getDp() );

    assertEquals( 0.4, roughnessCls.getEddyXX() );
    assertEquals( 0.5, roughnessCls.getMarsh() );

    assertEquals( "grass", //$NON-NLS-1$
        roughnessCls.getName() );
    // assertEquals(
    // "htpp://wwww.tuhh.de/wb/roughness_db/grass",
    // roughnessCls.getURI());
    assertNull( "", //$NON-NLS-1$
        roughnessCls.getDescription() );

    // setter
    roughnessCls.setAxAy( 0.11 );
    assertEquals( 0.11, roughnessCls.getAxAy() );

    roughnessCls.setDp( 0.22 );
    assertEquals( 0.22, roughnessCls.getDp() );

    roughnessCls.setEddyXX( 0.33 );
    assertEquals( 0.33, roughnessCls.getEddyXX() );

    roughnessCls.setKs( 0.44 );
    assertEquals( 0.44, roughnessCls.getKs() );

    roughnessCls.setMarsh( 0.55 );
    assertEquals( 0.55, roughnessCls.getMarsh() );
    final String NEW_NAME = "__name__"; //$NON-NLS-1$
    roughnessCls.setName( NEW_NAME );
    assertEquals( NEW_NAME, roughnessCls.getName() );

    // TODO check gml:id changing mechnanism and retest
    // final String URI="_URI_";
    // roughnessCls.setURI(URI);
    // assertEquals(URI, roughnessCls.getURI());
  }

  public void testCreation( ) throws Exception
  {
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( TestWorkspaces.URL_EMPTY_GML, null );

    final Feature root = workspace.getRootFeature();
    // gml:FeatureCollection

    final RoughnessCls r = (RoughnessCls) root;
    assertEquals( Double.NaN, r.getAxAy() );
    assertEquals( Double.NaN, r.getDp() );
    assertEquals( 500.0, r.getEddyXX() );
    assertEquals( Double.NaN, r.getKs() );
    assertEquals( Double.NaN, r.getMarsh() );
    assertNull( r.getDescription() );
    assertNull( r.getName() );
    // set and test
    r.setAxAy( 0.1 );
    assertEquals( 0.1, r.getAxAy() );

    r.setDp( 0.2 );
    assertEquals( 0.2, r.getDp() );

    r.setEddyXX( 0.3 );
    assertEquals( 0.3, r.getEddyXX() );

    r.setKs( 0.4 );
    assertEquals( 0.4, r.getKs() );

    r.setMarsh( 0.55555 );
    assertEquals( 0.55555, r.getMarsh() );

    r.setDescription( "D" ); //$NON-NLS-1$
    assertEquals( "D", r.getDescription() ); //$NON-NLS-1$

    r.setName( "N" ); //$NON-NLS-1$
    assertEquals( "N", r.getName() ); //$NON-NLS-1$
  }
}
