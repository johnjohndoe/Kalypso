package test.org.kalypso.kalypsosimulationmodel;

import java.util.Map;

import junit.framework.AssertionFailedError;
import junit.framework.TestCase;

import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureProvider;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Test the {@link org.kalypso.kalypsosimulationmodel.core.terrainmodel.IntersectionBasedRoughnessEstimate} 
 * 
 * 
 * @author Patrice Congo
 *
 */
public class TestRoughnessPolygonCollectionRoughnessEstimate extends TestCase
{

    /**
     * @see junit.framework.TestCase#setUp()
     */
    @Override
    protected void setUp( ) throws Exception
    {
      super.setUp();
    }
    
    public void testWithRP1Polygone()
    {
        GMLWorkspace roughnessWorkspace = null; 
        IFeatureProvider roughnessFeatureProvider;
        
        GMLWorkspace workspace=null;
        
        try
        {
          roughnessWorkspace =
            GmlSerializer.createGMLWorkspace( 
                TestWorkspaces.URL_ROUGHNESS_DB, 
                null );
          
            workspace=
                GmlSerializer.createGMLWorkspace( 
                    TestWorkspaces.URL_ROUGHNESS_POLYGON_COLLECTION_ESTIMATE, 
                    null );
            
        }
        catch(Throwable th)
        {
            fail(TestUtils.getStackTraceAsString(th));
        }
        try
        {
            Feature rootFeature = workspace.getRootFeature();
            IRoughnessPolygonCollection rpc = 
              (IRoughnessPolygonCollection)
                  rootFeature.getAdapter( IRoughnessPolygonCollection.class );
            assertNotNull( 
                "Could not adapt to IRoughnessPolygonCollection:" + rootFeature, 
                rpc );
            
            final double[] exteriorRP1 = { 0,0,0, 0,1,0, 1,1,0, 1,0,0, 0,0,0 };
            
            GM_Surface surfacePatch =
                rpc.get( 0 ).getSurface().getSurfaceAt( 0 );//makeSurface( exteriorRP1 );
//            System.out.println("area="+surfacePatch.getArea());
            final IRoughnessEstimateSpec estimate = 
                        rpc.getRoughnessEstimateSpec( surfacePatch );
            assertEquals( 
                "Only one roughness should be in the estimate", 
                1,
                estimate.getHistogramm().size() );
            
            assertEquals(
                "Most spread roughness must be roughness_cls1",
                "roughness_cls1",
                estimate.mostSpreadRoughness()[0].getGmlID());
            Double estimateFor1Roughness = 
                estimate.getHistogramm().values().iterator().next();
            assertTrue(
                "Estimate for roughness 1 must be nearly equals to 100%:"+estimateFor1Roughness,
                Math.abs( 1 - estimateFor1Roughness ) < 1e-6 );
        }
        catch ( Throwable th ) 
        {
          th.printStackTrace();
          fail(TestUtils.getStackTraceAsString(th));
        }
    }
    
    public void testWithRP1_2_3_Polygone()
    {
        GMLWorkspace workspace=null;
        
        try
        {
            workspace=
                GmlSerializer.createGMLWorkspace( 
                    TestWorkspaces.URL_ROUGHNESS_POLYGON_COLLECTION_ESTIMATE, 
                    null);
            
        }
        catch(Throwable th)
        {
            fail(TestUtils.getStackTraceAsString(th));
        }
        try
        {
            Feature rootFeature = workspace.getRootFeature();
            IRoughnessPolygonCollection rpc = 
              (IRoughnessPolygonCollection)
                  rootFeature.getAdapter( IRoughnessPolygonCollection.class );
            assertNotNull( 
                "Could not adapt to IRoughnessPolygonCollection:" + rootFeature, 
                rpc );
            
            final double[] exteriorRP1_2_3 = 
                { 0,0,0, -1,0,0, -1,-1,0, 0,-1,0, 1,0,0, 1,1,0, 0,1,0, 0,0,0 };
            
            GM_Surface surfacePatch =
                makeSurface( exteriorRP1_2_3 );
          
            final IRoughnessEstimateSpec estimate = 
                        rpc.getRoughnessEstimateSpec( surfacePatch );

            assertEquals( 
                "All 3 rougness must be should be in the estimate", 
                3,
                estimate.getHistogramm().size() );
             final double totalArea = computeTotalArea( rpc );
             Map<IRoughnessCls, Double> histogramm = estimate.getHistogramm();
             for(IRoughnessPolygon rp : rpc)
             {
               final double rpArea =computeTotalArea( rp );
               IRoughnessCls roughnessCls = rp.getRoughnessCls();
              final Double percentage = 
                 histogramm.get( roughnessCls );
               final String message =
                   String.format( 
                       "Testing \n\trp=%s\n\troughness=%s\n\trpArea%f \n\ttotalArea=%f \n\t percentage=%f",
                       rp.getGmlID(),
                       roughnessCls.getGmlID(),
                       rpArea,
                       totalArea ,
                       percentage );
               
              assertTrue(
                  message,
                  Math.abs( percentage - rpArea/totalArea)<1e-6 );
             }
             assertEquals( 
                 "2 most spread in the estimate zone",
                 2,
                 estimate.mostSpreadRoughness().length);
                 
             
        }
        catch (AssertionFailedError e) 
        {
          throw e;
        }
        catch ( Throwable th ) 
        {
          th.printStackTrace();
          fail(TestUtils.getStackTraceAsString(th));
        }
    }
    
    private static final double computeTotalArea(IRoughnessPolygonCollection rpc)
    {
      double totalCoveredArea = 0;
      for( final IRoughnessPolygon pol : rpc )
      {
        for( GM_Surface surface : pol.getSurface().getAllSurfaces() )
        {
          totalCoveredArea = totalCoveredArea + surface.getArea();
        }
      }
      return totalCoveredArea;
    }
    
    
    public void testNonOverlapping( )
    {
        GMLWorkspace workspace=null;
        
        try
        {
          workspace=
                GmlSerializer.createGMLWorkspace( 
                    TestWorkspaces.URL_ROUGHNESS_POLYGON_COLLECTION_ESTIMATE, 
                    null );
            
        }
        catch(Throwable th)
        {
            fail(TestUtils.getStackTraceAsString(th));
        }
        try
        {
            Feature rootFeature = workspace.getRootFeature();
            IRoughnessPolygonCollection rpc = 
              (IRoughnessPolygonCollection)
                  rootFeature.getAdapter( IRoughnessPolygonCollection.class );
            assertNotNull( 
                "Could not adapt to IRoughnessPolygonCollection:" + rootFeature, 
                rpc );
            
            final double[] norougnessZone = { 0,0,0, 0,1,0, -1,1,0, -1,0,0, 0,0,0 };
            
            GM_Surface surfacePatch = makeSurface( norougnessZone );
            
            final IRoughnessEstimateSpec estimate = 
                        rpc.getRoughnessEstimateSpec( surfacePatch );
            assertTrue( 
                "Given estimate zone does not overlap with the roughness, so histogram must be empty", 
                estimate.getHistogramm().isEmpty() );
            
        }
        catch ( Throwable th ) 
        {
          fail(TestUtils.getStackTraceAsString(th));
        }
    }
    
    private static final double computeTotalArea(IRoughnessPolygon rp)
    {
      double totalCoveredArea = 0;
      for( GM_Surface surface : rp.getSurface().getAllSurfaces() )
      {
        totalCoveredArea = totalCoveredArea + surface.getArea();
      }      
      return totalCoveredArea;
    }
	private static final GM_Surface makeSurface( double[] exterior ) throws GM_Exception
	{
      GM_Surface surfacePatch = 
        GeometryFactory.createGM_Surface( 
              exterior, 
              TestWorkspaces.NO_INTERIOR, 
              3, 
              TestWorkspaces.getGaussKrueger());
      return surfacePatch;
    }
}
