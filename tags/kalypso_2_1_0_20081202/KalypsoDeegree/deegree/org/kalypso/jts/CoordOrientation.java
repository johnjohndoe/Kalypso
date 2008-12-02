package org.kalypso.jts;

import java.util.logging.Logger;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateArrays;

/**
 * Hilfsklasse zum Orientieren von Coordinatenringen
 * 
 * @author belger
 */
public class CoordOrientation
{
  private static final Logger LOG = Logger.getLogger( CoordOrientation.class.getName() );

  public static class TYPE
  {
    public static TYPE POSITIV = new TYPE( "positiv" );

    public static TYPE NEGATIV = new TYPE( "negativ" );

    private String m_name;

    private TYPE( final String name )
    {
      m_name = name;
    }

    @Override
    public String toString( )
    {
      return m_name;
    }
  }

  public static void orient( final Coordinate[] coords, final TYPE type ) throws CoordOrientationException
  {
    if( orientation2D_Polygon( coords ) != type )
      CoordinateArrays.reverse( coords );
  }

  /**
   * Orientation of 2D-Polygon, taken from http://geometryalgorithms.com/Archive/algorithm_0101/algorithm_0101.htm
   * Original comment: orientation2D_Polygon(): tests the orientation of a simple polygon Input: int n = the number of
   * vertices in the polygon Point* V = an array of n+1 vertices with V[n]=V[0] Return: >0 for counterclockwise =0 for
   * none (degenerate) <0 for clockwise Note: this algorithm is faster than computing the signed area.
   * 
   * @throws CoordOrientationException
   */
  public static TYPE orientation2D_Polygon( final Coordinate[] coords ) throws CoordOrientationException
  {
    if( coords == null || coords.length < 3 || coords[0] != coords[coords.length - 1] )
      LOG.warning( "unable to filter coords: coord do not form a LinearRing" );

    // first find rightmost lowest vertex of the polygon
    int rmin = 0;
    double xmin = coords[0].x;
    double ymin = coords[0].y;

    for( int i = 1; i < coords.length - 1; i++ )
    {
      if( coords[i].y > ymin )
        continue;

      if( coords[i].y == ymin )
      {
        // just as low
        if( coords[i].x < xmin ) // and to left
          continue;
      }

      rmin = i; // a new rightmost lowest vertex
      xmin = coords[i].x;
      ymin = coords[i].y;
    }

    final Coordinate P0 = rmin == 0 ? coords[coords.length - 2] : coords[rmin - 1];
    final Coordinate P1 = coords[rmin];
    final Coordinate P2 = coords[rmin + 1];

    final double o = (P1.x - P0.x) * (P2.y - P0.y) - (P2.x - P0.x) * (P1.y - P0.y);

    if( o < 0 )
      return TYPE.NEGATIV;
    else if( o > 0 )
      return TYPE.POSITIV;
    else
      throw new CoordOrientationException( "Anwendungsfehler: degeneriertes Polygon aufgetreten" );
  }

}
