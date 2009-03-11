package org.kalypsodeegree_impl.tools.refinement;

import junit.framework.TestCase;

import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Thomas Jung
 */
public class RefinmentTest extends TestCase
{

  @SuppressWarnings("unchecked")
  public void testLoadResults( ) throws Exception
  {
    final String crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    final GM_Position[] positions = new GM_Position[5];

    /* test polygon */
    positions[0] = GeometryFactory.createGM_Position( 0, 0, 0 );
    positions[1] = GeometryFactory.createGM_Position( 1, 0, 0 );
    positions[2] = GeometryFactory.createGM_Position( 1, 1, 2 );
    positions[3] = GeometryFactory.createGM_Position( 0, 1, 1 );
    positions[4] = GeometryFactory.createGM_Position( 0, 0, 0 );

    final GM_Ring exterior = GeometryFactory.createGM_Ring( positions, crs );
    final GM_Ring[] interior = null;

    final GM_SurfacePatch patch = GeometryFactory.createGM_SurfacePatch( exterior, interior, crs );

    final GM_Surface<GM_SurfacePatch> surface = GeometryFactory.createGM_Surface( patch );
    final GM_Surface[] surfaces = new GM_Surface[] { surface };

    final GM_MultiSurface multiSurface = GeometryFactory.createGM_MultiSurface( surfaces, crs );

    /* test curve 1 */
    final GM_Position[] curvePositions = new GM_Position[2];
    curvePositions[0] = GeometryFactory.createGM_Position( 0, 0, 0 );
    curvePositions[1] = GeometryFactory.createGM_Position( 1, 2, 0 );

    System.out.format( "curve1\n" );

    final GM_Curve curve = GeometryFactory.createGM_Curve( curvePositions, crs );

    final GM_MultiSurface[] multiSurfaces = new GM_MultiSurface[] { multiSurface };
    final Refinement refinement = new Refinement();
    final GM_Object[] doRefine = refinement.doRefine( multiSurfaces, curve );

    for( final GM_Object object : doRefine )
    {
      if( object instanceof GM_Surface )
      {
        final GM_Surface<GM_SurfacePatch> surface1 = (GM_Surface<GM_SurfacePatch>) object;
        for( final GM_SurfacePatch surfacePatch : surface1 )
        {
          final GM_Position[] ring = surfacePatch.getExteriorRing();
          for( int i = 0; i < ring.length; i++ )
          {
            System.out.format( "ring # %d", i );
            System.out.format( "%9.2f %9.2f %9.2f \n", ring[i].getX(), ring[i].getY(), ring[i].getZ() );
          }
        }
      }
    }
    /* test curve 2 */
    final GM_Position[] curvePositions2 = new GM_Position[2];
    curvePositions2[0] = GeometryFactory.createGM_Position( 0, 0, 0 );
    curvePositions2[1] = GeometryFactory.createGM_Position( 2, 2, 0 );

    System.out.format( "curve2\n" );

    final GM_Curve curve2 = GeometryFactory.createGM_Curve( curvePositions2, crs );

    final GM_Object[] doRefine2 = refinement.doRefine( multiSurfaces, curve2 );

    for( final GM_Object object : doRefine2 )
    {
      if( object instanceof GM_Surface )
      {
        final GM_Surface<GM_SurfacePatch> surface2 = (GM_Surface<GM_SurfacePatch>) object;
        for( final GM_SurfacePatch surfacePatch : surface2 )
        {
          final GM_Position[] ring = surfacePatch.getExteriorRing();
          for( int i = 0; i < ring.length; i++ )
          {
            System.out.format( "ring # %d", i );
            System.out.format( "%9.2f %9.2f %9.2f \n", ring[i].getX(), ring[i].getY(), ring[i].getZ() );
          }
        }
      }
    }

    /* test curve 3 */
    final GM_Position[] curvePositions3 = new GM_Position[2];
    curvePositions3[0] = GeometryFactory.createGM_Position( 0, 0, 0 );
    curvePositions3[1] = GeometryFactory.createGM_Position( 0, 1, 0 );

    System.out.format( "curve3\n" );

    final GM_Curve curve3 = GeometryFactory.createGM_Curve( curvePositions3, crs );

    final GM_Object[] doRefine3 = refinement.doRefine( multiSurfaces, curve3 );

    for( final GM_Object object : doRefine3 )
    {
      if( object instanceof GM_Surface )
      {
        final GM_Surface<GM_SurfacePatch> surface3 = (GM_Surface<GM_SurfacePatch>) object;
        for( final GM_SurfacePatch surfacePatch : surface3 )
        {
          final GM_Position[] ring = surfacePatch.getExteriorRing();
          for( int i = 0; i < ring.length; i++ )
          {
            System.out.format( "pos # %d:  ", i );
            System.out.format( "%9.2f %9.2f %9.2f \n", ring[i].getX(), ring[i].getY(), ring[i].getZ() );
          }
        }
      }
    }
    /* test curve 4 */
    final GM_Position[] curvePositions4 = new GM_Position[2];
    curvePositions4[0] = GeometryFactory.createGM_Position( -0.50, 0, 0 );
    curvePositions4[1] = GeometryFactory.createGM_Position( 1, 1.5, 0 );

    System.out.format( "curve4\n" );

    final GM_Curve curve4 = GeometryFactory.createGM_Curve( curvePositions4, crs );

    final GM_Object[] doRefine4 = refinement.doRefine( multiSurfaces, curve4 );

    for( final GM_Object object : doRefine4 )
    {
      if( object instanceof GM_Surface )
      {
        final GM_Surface<GM_SurfacePatch> surface4 = (GM_Surface<GM_SurfacePatch>) object;
        for( final GM_SurfacePatch surfacePatch : surface4 )
        {
          final GM_Position[] ring = surfacePatch.getExteriorRing();

          if( ring.length > 4 )
          {
            // split again
            // right now: simple polygon triangulation
            // make a polygon from the curves (polygon must be oriented ccw)
            final GM_Surface<GM_SurfacePatch>[] triangulatedPolygon = RefinementUtils.triangulatePolygon( crs, ring );
            for( final GM_Surface<GM_SurfacePatch> triangle : triangulatedPolygon )
            {
              for( final GM_SurfacePatch trianglePatches : triangle )
              {
                final GM_Position[] trianglePoses = trianglePatches.getExteriorRing();
                for( int i = 0; i < trianglePoses.length; i++ )
                {
                  System.out.format( "pos # %d:  ", i );
                  System.out.format( "%9.2f %9.2f %9.2f \n", trianglePoses[i].getX(), trianglePoses[i].getY(), trianglePoses[i].getZ() );
                }
              }
            }
          }
          else
          {
            for( int i = 0; i < ring.length; i++ )
            {
              System.out.format( "pos # %d:  ", i );
              System.out.format( "%9.2f %9.2f %9.2f \n", ring[i].getX(), ring[i].getY(), ring[i].getZ() );
            }
          }
        }
      }
    }
    /* test curve 5 */
    final GM_Position[] curvePositions5 = new GM_Position[2];
    curvePositions5[0] = GeometryFactory.createGM_Position( 0.50, 0.5, 0 );
    curvePositions5[1] = GeometryFactory.createGM_Position( 0.50, 1.5, 0 );

    System.out.format( "curve5\n" );

    final GM_Curve curve5 = GeometryFactory.createGM_Curve( curvePositions5, crs );

    final GM_Object[] doRefine5 = refinement.doRefine( multiSurfaces, curve5 );

    for( final GM_Object object : doRefine5 )
    {
      if( object instanceof GM_Surface )
      {
        final GM_Surface<GM_SurfacePatch> surface5 = (GM_Surface<GM_SurfacePatch>) object;
        for( final GM_SurfacePatch surfacePatch : surface5 )
        {
          final GM_Position[] ring = surfacePatch.getExteriorRing();
          for( int i = 0; i < ring.length; i++ )
          {
            System.out.format( "pos # %d:  ", i );
            System.out.format( "%9.2f %9.2f %9.2f \n", ring[i].getX(), ring[i].getY(), ring[i].getZ() );
          }
        }
      }
    }

  }

}