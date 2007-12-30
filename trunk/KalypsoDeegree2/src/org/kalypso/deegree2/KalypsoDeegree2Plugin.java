package org.kalypso.deegree2;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.Plugin;
import org.j3d.geom.TriangulationUtils;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle.
 */
public class KalypsoDeegree2Plugin extends Plugin
{

  /**
   * The plug-in ID.
   */
  public static final String PLUGIN_ID = "org.kalypso.degree2";

  /**
   * The shared instance.
   */
  private static KalypsoDeegree2Plugin plugin;

  /**
   * The constructor.
   */
  public KalypsoDeegree2Plugin( )
  {
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.core.runtime.Plugins#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( BundleContext context ) throws Exception
  {
    super.start( context );

    plugin = this;

    org.j3d.geom.TriangulationUtils t;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.core.runtime.Plugin#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( BundleContext context ) throws Exception
  {
    plugin = null;

    super.stop( context );
  }

  /**
   * Returns the shared instance.
   * 
   * @return The shared instance.
   */
  public static KalypsoDeegree2Plugin getDefault( )
  {
    return plugin;
  }

  /**
   * Triangulates any arbitrary concave polygon. For the triangulation of the polygon an algorithm delivered with j3d
   * (Seidel algorithm) is used. This algorithm needs a data structure as follows: <BR>- coords of the polygon points
   * as a float array ( x1,y1,z1,x2,y2,z2,x3...) <BR>- polygon oriented ccw <br>- closed polygon not needed. <br>
   * It returns the order of the polygon point coords and the number of triangles. <br>
   * TODO!: move into real helper class
   * 
   * @param coords
   *            float-array of polygons coords
   * @return Map with number of triangles as key and coordinate order as value
   */
  public static Map<Integer, int[]> doTriangle( float[] coords )
  {
    TriangulationUtils triangulator = new TriangulationUtils();

    float[] normal = { 0, 0, 1 };
    int[] output = new int[coords.length];
    int numVertices = coords.length / 3;
// for( int i = 0; i < coords.length; )
// {
// System.out.print( i );
// System.out.print( " " );
// System.out.print( coords[i++] );
// System.out.print( " " );
// System.out.print( coords[i++] );
// System.out.print( " " );
// System.out.print( coords[i++] );
// System.out.println();
// }
//
    int num = triangulator.triangulateConcavePolygon( coords, 0, numVertices, output, normal );
// System.out.println( "number of triangles = " + num );

// for( int i = 0; i < num; i++ )
// {
// System.out.print( i );
// System.out.print( ": " );
// System.out.print( output[i * 3] );
// System.out.print( " " );
// System.out.print( output[i * 3 + 1] );
// System.out.print( " " );
// System.out.print( output[i * 3 + 2] );
// System.out.print( " c " );
// System.out.print( coords[output[i * 3]] );
// System.out.print( " " );
// System.out.print( coords[output[i * 3] + 1] );
// System.out.print( " " );
// System.out.print( coords[output[i * 3] + 2] );
// System.out.print( ", " );
// System.out.print( coords[output[i * 3 + 1]] );
// System.out.print( " " );
// System.out.print( coords[output[i * 3 + 1] + 1] );
// System.out.print( " " );
// System.out.print( coords[output[i * 3 + 1] + 2] );
// System.out.print( ", " );
// System.out.print( coords[output[i * 3 + 2]] );
// System.out.print( " " );
// System.out.print( coords[output[i * 3 + 2] + 1] );
// System.out.print( " " );
// System.out.print( coords[output[i * 3 + 2] + 2] );
// System.out.println();
// }

    final Map<Integer, int[]> outputList = new HashMap<Integer, int[]>();
    // return the triangles
    outputList.put( num, output );
    return outputList;
  }
}