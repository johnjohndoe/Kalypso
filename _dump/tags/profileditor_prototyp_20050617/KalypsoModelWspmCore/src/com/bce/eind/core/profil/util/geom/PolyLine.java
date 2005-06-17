package com.bce.eind.core.profil.util.geom;

import java.awt.geom.Point2D;
import java.util.Arrays;
import java.util.NoSuchElementException;
import java.util.Vector;

import com.bce.eind.core.profil.util.DoubleComparator;
import com.bce.eind.core.profil.util.Range;
import com.bce.eind.core.profil.util.math.LinearEquation;

/**
 * Represents a curve made of lines, each of them being connected together, except (but it can also
 * be the case) for the first and the last lines.
 * <p>
 * NOTE: the class is currently under redesign, points are stored as Point2D and also as
 * coordinates. This is maybe not the best solution. But it allows us to perform some operations
 * more rapidly.
 * </p>
 * 
 * @author schlienger
 */
public class PolyLine
{
  /** P1 must be over P2 */
  public static final int OVER = 4;

  /** P1 must be under P2 */
  public static final int UNDER = 2;

  /** P1 can be over and under P2 */
  public static final int ALL = OVER | UNDER;

  /** X coordinates, copy of the X-coords of m_pts */
  private double[] m_x;

  /** Y coordinates, copy of the Y-coords of m_pts */
  private double[] m_y;

  /** used for double comparisons */
  private final double m_delta;

  /**
   * Constructor. Calls setPoints( Point2D[] ).
   * 
   * @param points
   * @param delta
   *          used for double comparison
   */
  public PolyLine( Point2D[] points, double delta )
  {
    m_delta = delta;

    setPoints( points );
  }

  /**
   * Constructor. Calls setPoints( double[], double[] ).
   * 
   * @param x
   * @param y
   * @param delta
   *          used for double comparison
   */
  public PolyLine( double[] x, double[] y, double delta )
  {
    m_delta = delta;

    setPoints( x, y );
  }

  /**
   * Constructor. Calls setPoints( Double[], Double[] ).
   * 
   * @param x
   * @param y
   * @param delta
   *          used for double comparison
   */
  public PolyLine( Double[] x, Double[] y, double delta )
  {
    m_delta = delta;

    setPoints( x, y );
  }

  /**
   * constructor. calls setPoints( int[], int[])
   * 
   * @param x
   * @param y
   * @param delta
   *          used for double comparison
   */
  public PolyLine( int[] x, int[] y, double delta )
  {
    m_delta = delta;

    setPoints( x, y );
  }

  /**
   * Sets the points. Points will be sorted on X before being set.
   * 
   * @param pts
   *          order is not important. Method sorts them on X coordinate.
   */
  public void setPoints( final Point2D[] pts )
  {
    PointUtilities.sortX( pts, m_delta );

    m_x = new double[pts.length];
    m_y = new double[pts.length];

    for( int i = 0; i < pts.length; i++ )
    {
      m_x[i] = pts[i].getX();
      m_y[i] = pts[i].getY();
    }
  }

  /**
   * Sets the points. Points will be sorted on X before being set.
   * 
   * @param x
   *          X coordinates
   * @param y
   *          Y coordinates
   */
  public void setPoints( double[] x, double[] y )
  {
    Point2D[] pts = new Point2D.Double[x.length];

    for( int i = 0; i < pts.length; i++ )
      pts[i] = new Point2D.Double( x[i], y[i] );

    setPoints( pts );
  }

  /**
   * Sets the points. Points will be sorted on X before being set.
   * 
   * @param x
   *          X coordinates
   * @param y
   *          Y coordinates
   */
  public void setPoints( Double[] x, Double[] y )
  {
    Point2D[] pts = new Point2D.Double[x.length];

    for( int i = 0; i < pts.length; i++ )
      pts[i] = new Point2D.Double( x[i].doubleValue(), y[i].doubleValue() );

    setPoints( pts );
  }

  /**
   * Sets the points. Points will be sorted on X before being set.
   * 
   * @param x
   *          X coordinates
   * @param y
   *          Y coordinates
   */
  public void setPoints( int[] x, int[] y )
  {
    Point2D[] pts = new Point2D.Double[x.length];

    for( int i = 0; i < pts.length; i++ )
      pts[i] = new Point2D.Double( x[i], y[i] );

    setPoints( pts );
  }

  /**
   * Shortens this polyline and returns new one. It only takes the values in the given range of the
   * X Axis. If from and/or to is/are not present (Double.NaN), it computes the missing points and
   * insert them into the new line.
   * 
   * @param from
   * @param to
   * @return
   */
  public PolyLine shorten( double from, double to )
  {
    int begin = 0;
    int end = m_x.length - 1;

    if( Double.compare( from, Double.NaN ) != 0 )
      begin = Arrays.binarySearch( m_x, Math.min( from, to ) );

    if( Double.compare( to, Double.NaN ) != 0 )
      end = Arrays.binarySearch( m_x, Math.max( from, to ) );

    double newX1 = Double.NaN;
    double newY1 = Double.NaN;
    double newX2 = Double.NaN;
    double newY2 = Double.NaN;

    if( begin < 0 )
    {
      begin = -begin - 1;
      newX1 = Math.min( from, to );
      newY1 = getYFor( newX1, true );
    }

    if( end < 0 )
    {
      end = -end - 1;
      newX2 = Math.max( from, to );
      newY2 = getYFor( newX2, true );
    }
    else
      end++;

    boolean noX1 = Double.compare( newX1, Double.NaN ) == 0;
    boolean noX2 = Double.compare( newX2, Double.NaN ) == 0;

    int lengthToCopy = end - begin;
    int lengthToMake = lengthToCopy + (noX1 ? 0 : 1) + (noX2 ? 0 : 1);

    double[] x = new double[lengthToMake];
    double[] y = new double[lengthToMake];

    int whereToBegin = (noX1 ? 0 : 1);

    System.arraycopy( m_x, begin, x, whereToBegin, lengthToCopy );
    System.arraycopy( m_y, begin, y, whereToBegin, lengthToCopy );

    if( !noX1 )
    {
      x[0] = newX1;
      y[0] = newY1;
    }

    if( !noX2 )
    {
      x[lengthToMake - 1] = newX2;
      y[lengthToMake - 1] = newY2;
    }

    return new PolyLine( x, y, m_delta );
  }

  /**
   * return the Y for a given X. Interpolation is done when:<br> - no point.X is found that equals
   * this X and<br> - X is not bigger than all point.X
   * 
   * @param X -
   *          for which to find a Y
   * @return - either existing Y or linear interpolated Y
   */
  public double getYFor( double X )
  {
    return getYFor( X, false );
  }

  /**
   * Gibt Y Wert an einer bestimmten Stelle der PolyLine zurück. Der Wert wir linear zwischen zwei
   * Stützpunkten interpoliert. Funktioniert nur, wenn x-Wert austeigend sortiert.
   * 
   * @param X
   *          Für diesen x Wert wird der y-Wert gesucht
   * @param bExtrapol
   *          Für Punkte ausserhalb der Polylinie: falls true wird der Rand aus den beiden
   *          letzten/ersten Punkten extrapoliert, bei false wird die PolyLine wagerecht mit der
   *          Höhe des ersten/letzten Punktes fortgesetzt.
   * @return Interpoliert y-Wert für X
   */
  public double getYFor( double X, boolean bExtrapol )
  {
    int pos = Arrays.binarySearch( m_x, X );

    if( pos < 0 )
    {
      pos = Math.abs( pos ) - 1;

      if( pos == 0 )
      {
        if( bExtrapol )
          pos++;
        else

          return m_y[pos];
      }
      else if( pos == m_x.length )
      {
        if( bExtrapol )
          pos -= 1;
        else

          return m_y[pos - 1];
      }

      // linear Interpolieren
      double x1 = m_x[pos - 1];
      double y1 = m_y[pos - 1];
      double x2 = m_x[pos];
      double y2 = m_y[pos];

      try
      {
        LinearEquation eq = new LinearEquation( x1, y1, x2, y2 );

        return eq.computeY( X );
      }
      catch( LinearEquation.SameXValuesException e )
      {
        return y1;
      }
    }

    return m_y[pos];
  }

  /**
   * UGLY: same method as getYFor(), but returns a Point2D instead. This code could be ameliorated
   * by mergin both methods.
   * 
   * @param X
   * @param bExtrapol
   * @return
   * @see PolyLine#getYFor(double, boolean)
   */
  public Point2D getPointFor( double X, boolean bExtrapol )
  {
    int pos = Arrays.binarySearch( m_x, X );

    if( pos < 0 )
    {
      pos = Math.abs( pos ) - 1;

      if( pos == 0 )
      {
        if( bExtrapol )
          pos++;
        else

          return new Point2D.Double( m_x[pos], m_y[pos] );
      }
      else if( pos == m_x.length )
      {
        if( bExtrapol )
          pos -= 1;
        else

          return new Point2D.Double( m_x[pos - 1], m_y[pos - 1] );
      }

      // linear Interpolieren
      double x1 = m_x[pos - 1];
      double y1 = m_y[pos - 1];
      double x2 = m_x[pos];
      double y2 = m_y[pos];

      try
      {
        LinearEquation eq = new LinearEquation( x1, y1, x2, y2 );

        return new Point2D.Double( X, eq.computeY( X ) );
      }
      catch( LinearEquation.SameXValuesException e )
      {
        return new Point2D.Double( x1, y1 );
      }
    }

    return new Point2D.Double( m_x[pos], m_y[pos] );
  }

  /**
   * @see PolyLine#area(double[], PolyLine, PolyLine, double, double, int)
   */
  public static double area( final PolyLine p1, final PolyLine p2, final double from,
      final double to, final int mode )
  {
    return area( p1.shorten( from, to ), p2.shorten( from, to ), mode );
  }

  /**
   * Computes the area between two polyline
   * 
   * @param xe
   *          [null allowed] the refined X-coords of both p1 and p2, need to be sorted. When null is
   *          given, then xe is computed internally
   * @param p1
   * @param p2
   * @param mode
   * @return computed area
   * @throws IllegalArgumentException
   *           if mode is not valid
   */
  public static double area( PolyLine p1, PolyLine p2, int mode )
  {
    // mode verification
    if( (mode != OVER) && (mode != UNDER) && (mode != ALL) )
      throw new IllegalArgumentException( "ungültiges Flächenberechnungsmodus" );

    // if no refined coordinates are given, we compute them here
    double[] xe = PolyLine.refinePolylines( p1, p2 );
    Arrays.sort( xe );
    xe = com.bce.eind.core.profil.util.Arrays.removeDupicates( xe, p1.m_delta );

    // trapeze and points used to compute area
    Trapeze trap = new Trapeze();
    Point2D pt11 = new Point2D.Double();
    Point2D pt12 = new Point2D.Double();
    Point2D pt21 = new Point2D.Double();
    Point2D pt22 = new Point2D.Double();

    double area = 0;

    DoubleComparator dc = new DoubleComparator( p1.m_delta );

    for( int i = 0 /* startX */; i < (xe.length - 1) /* ( stopX - 1 ) */; i++ )
    {
      double x1 = xe[i];
      double x2 = xe[i + 1];

      /*
       * TRICKY: compute area only when points' placement verifies mode.
       */
      double y11 = p1.getYFor( x1 );
      double y12 = p2.getYFor( x1 );
      double y21 = p1.getYFor( x2 );
      double y22 = p2.getYFor( x2 );

      // if( ( ( mode == OVER ) && ( ( y11 - y12 ) >= -Y_PREC ) &&
      // ( ( y21 - y22 ) >= -Y_PREC ) ) ||
      // ( ( mode == UNDER ) && ( ( y12 - y11 ) >= -Y_PREC ) &&
      // ( ( y22 - y21 ) >= -Y_PREC ) ) || ( mode == ALL ) )
      if( ((mode == OVER) && (dc.compare( y11, y12 ) >= 0) && (dc.compare( y21, y22 ) >= 0))
          || ((mode == UNDER) && (dc.compare( y12, y11 ) >= 0) && (dc.compare( y22, y21 ) >= 0))
          || (mode == ALL) )
      {
        pt11.setLocation( x1, y11 );
        pt12.setLocation( x1, y12 );

        pt21.setLocation( x2, y21 );
        pt22.setLocation( x2, y22 );

        trap.setPoints( pt11, pt12, pt21, pt22 );

        area += trap.area();
      }
    }

    return area;
  }

  /**
   * Returns all x-Values where this PolyLine intersect another.
   * 
   * @param other
   *          PolyLine to intersect
   * @return x-Values where this and other intersect
   */
  public double[] intersect( PolyLine other )
  {
    // we expect no more than maximum of points intersections
    double[] xe = new double[getX().length + other.getX().length];
    int xPos = 0;

    DoubleComparator dc = new DoubleComparator( m_delta );

    // declared here for object creation reduction in loops, LinearEquation.setPoints() is used
    LinearEquation l1 = new LinearEquation();
    LinearEquation l2 = new LinearEquation();

    // find crossings
    for( int i = 0; i < (m_x.length - 1); i++ )
    {
      double x11 = m_x[i];
      double y11 = m_y[i];
      double x12 = m_x[i + 1];
      double y12 = m_y[i + 1];

      try
      {
        l1.setPoints( x11, y11, x12, y12 );
      }
      catch( LinearEquation.SameXValuesException e )
      {
        // x-coords are the same
        continue;
      }

      int startJ = Math.max( 0, Math.abs( Arrays.binarySearch( other.m_x, x11 ) ) - 2 );
      int stopJ = Math
          .min( other.m_x.length, Math.abs( Arrays.binarySearch( other.m_x, x12 ) ) + 2 );

      for( int j = startJ; j < (stopJ - 1); j++ )
      {
        try
        {
          // try to intersect the two segments
          l2.setPoints( other.m_x[j], other.m_y[j], other.m_x[j + 1], other.m_y[j + 1] );

          // intersect the two segments
          double x = l1.solve( l2 );

          if( (dc.compare( x11, x ) <= 0) && (dc.compare( x, x12 ) <= 0) )
            xe[xPos++] = x;
        }
        catch( NoSuchElementException ignored )
        {
          // no intersection found, continue
        }
        catch( LinearEquation.SameXValuesException ile )
        {
          // x1 == x2, continue
        }
      }
    }

    // copy to shorten array
    double[] x = new double[xPos];
    System.arraycopy( xe, 0, x, 0, xPos );

    return x;
  }

  /**
   * Returns the intersections (PolyLines) where pOther intersects pMaster. Only the intersections
   * of pOther that correspond to mode in regard to pMaster will be returned.
   * 
   * @param pMaster
   * @param pOther
   * @param mode
   * @return
   */
  public static PolyLine[] intersectAsPolyLines( PolyLine pMaster, PolyLine pOther, int mode )
  {
    // intersections between Geleande and WSP line
    double[] xis = pMaster.intersect( pOther );
    Arrays.sort( xis );

    // add extremum to intersections
    final Range r1 = pMaster.xExtremum();
    final Range r2 = pOther.xExtremum();

    final Range r = Range.mergeWide( r1, r2 );

    double[] ints = new double[xis.length + 2];
    ints[0] = r.getFrom();
    ints[ints.length - 1] = r.getTo();
    System.arraycopy( xis, 0, ints, 1, xis.length );

    ints = com.bce.eind.core.profil.util.Arrays.removeDupicates( ints, pMaster.m_delta );

    if( ints.length == 0 )
    {
      /*
       * no intersection points, let check the position of the Y-coord for one valid point and
       * decide whether or not to build polygone
       */
      if( isModeVerified( mode, pMaster, pOther ) )
        return new PolyLine[]
        { pOther };

      return new PolyLine[0];
    }

    final Vector<PolyLine> vPols = new Vector<PolyLine>( ints.length );

    // for each intersection
    for( int i = 0; i < (ints.length - 1); i++ )
    {
      if( isModeVerified( mode, pMaster, pOther, ints[i], ints[i + 1] ) )
      {
        PolyLine sp = pOther.shorten( ints[i], ints[i + 1] );

        vPols.add( sp );
      }
    }

    return vPols.toArray( new PolyLine[vPols.size()] );
  }

  /**
   * Gibt die Vereinigung der X-Werte zweier Polylines sowie deren Schnittpunkte zurück.
   * 
   * @return die (unsortierten) x-Werte: alle x-Werte von p1, p2 und vom Schnitt der beiden Polygone
   */
  public static double[] refinePolylines( final PolyLine p1, final PolyLine p2 )
  {
    // crossings between p1 and p2
    double[] interx = p1.intersect( p2 );

    // get all points from the union (on X) (use double[] instead of SortedSet because of
    // performance)
    double[] xe = new double[(Math.max( p1.getX().length, p2.getX().length ) * 2) + interx.length];

    int xPos = 0;

    // first line
    for( int i = 0; i < p1.m_x.length; i++ )
      xe[xPos++] = p1.m_x[i];

    // second line
    for( int i = 0; i < p2.m_x.length; i++ )
      xe[xPos++] = p2.m_x[i];

    // intersection points
    for( int i = 0; i < interx.length; i++ )
      xe[xPos++] = interx[i];

    // fit size
    double[] x = new double[xPos];
    System.arraycopy( xe, 0, x, 0, xPos );

    return x;
  }

  /**
   * builds the necessary polygones to represent the area between the two polylines. Depending on
   * the mode, it might either use all the points of the polylines or just the ones that correspond
   * to the Y-coord positioning.
   * 
   * @param p1
   *          the first polyline
   * @param p2
   *          the second polyline
   * @param mode
   *          position of p1 in regards to p2 (
   * @return
   * @throws IllegalArgumentException
   *           when mode is not one of ALL, OVER, UNDER.
   * @see PolyLine.ALL, OVER, UNDER)
   */
  public static PolyGone[] spaceBetween( final PolyLine p1, final PolyLine p2, int mode )
  {
    if( mode == PolyLine.ALL )

      return new PolyGone[]
      { buildPolyGone( p1, p2 ) };

    else if( (mode == PolyLine.OVER) || (mode == PolyLine.UNDER) )
    {
      // look for intersections, sort and remove duplicates
      double[] xis = p1.intersect( p2 );
      Arrays.sort( xis );
      xis = com.bce.eind.core.profil.util.Arrays.removeDupicates( xis, p1.m_delta );

      if( xis.length == 0 )
      {
        /*
         * no intersection points, let check the position of the Y-coord for one valid point and
         * decide whether or not to build polygone
         */
        if( isModeVerified( mode, p1, p2 ) )
          return new PolyGone[]
          { buildPolyGone( p1, p2 ) };

        return new PolyGone[0];
      }

      /*
       * if we are here, we have intersections
       */

      // add extremum to intersections
      Range r1 = p1.xExtremum();
      Range r2 = p2.xExtremum();

      Range r = Range.mergeWide( r1, r2 );

      double[] ints = new double[xis.length + 2];
      ints[0] = r.getFrom();
      ints[ints.length - 1] = r.getTo();
      System.arraycopy( xis, 0, ints, 1, xis.length );

      final Vector<PolyGone> vPols = new Vector<PolyGone>( ints.length );

      // for each intersection
      for( int i = 0; i < (ints.length - 1); i++ )
      {
        if( isModeVerified( mode, p1, p2, ints[i], ints[i + 1] ) )
        {
          PolyLine sp1 = p1.shorten( ints[i], ints[i + 1] );
          PolyLine sp2 = p2.shorten( ints[i], ints[i + 1] );

          vPols.add( buildPolyGone( sp1, sp2 ) );
        }
      }

      return vPols.toArray( new PolyGone[vPols.size()] );
    }
    else
      throw new IllegalArgumentException( "unvalid mode" );
  }

  /**
   * Builds a polygone with two polylines
   * 
   * @param p1
   * @param p2
   * @return
   */
  public static PolyGone buildPolyGone( PolyLine p1, PolyLine p2 )
  {
    // create a polygone with the first polyline
    PolyGone p = new PolyGone( p1.getX(), p1.getY() );

    // add the points of the second polyline (starting at the end: inver = true)
    p.addPoints( p2, true );

    return p;
  }

  /**
   * Tells if the polyline placement mode is verified according to the given polylines.
   * Pre-condition: the given polylines should not intersect themselves.
   * 
   * @param mode
   * @param p1
   * @param p2
   * @return
   */
  private static boolean isModeVerified( int mode, PolyLine p1, PolyLine p2 )
  {
    DoubleComparator dc = new DoubleComparator( p1.m_delta );

    if( (p1.m_y.length == 0) || (p2.m_y.length == 0) )
      return false;

    double[] xs = refinePolylines( p1, p2 );
    Arrays.sort( xs );
    xs = com.bce.eind.core.profil.util.Arrays.removeDupicates( xs, p1.getDelta() );

    for( int i = 0; i < xs.length; i++ )
    {
      // compare the points at this X-coordinate of both polylines
      double y1 = p1.getYFor( xs[i] );
      double y2 = p2.getYFor( xs[i] );

      if( (mode == ALL) || ((mode == PolyLine.OVER) && (dc.compare( y1, y2 ) > 0))
          || ((mode == PolyLine.UNDER) && (dc.compare( y1, y2 ) < 0)) )

        return true;
    }

    return false;
  }

  /**
   * Tells if the mode is verified according to the placement of the given polylines, in the range
   * from-to.<br>
   * 
   * @param mode
   * @param p1
   * @param p2
   * @param xFrom
   * @param xTo
   * @return
   */
  private static boolean isModeVerified( int mode, PolyLine p1, PolyLine p2, double xFrom,
      double xTo )
  {
    DoubleComparator dc = new DoubleComparator( p1.m_delta );

    if( (p1.m_y.length == 0) || (p2.m_y.length == 0) )
      return false;

    /*
     * take a point somewhere in the middle of the current segment it is possible that this trick
     * won't work all the time, for instance if y1 == y2 at x, then we will return false if mode ==
     * OVER or UNDER. This could be improved, for instance by trying more that one x-coord.
     */
    double x = ((xTo - xFrom) / 2) + xFrom;

    double y1 = p1.getYFor( x );
    double y2 = p2.getYFor( x );

    // compare the points at this X-coordinate of both polylines
    if( (mode == ALL) || ((mode == PolyLine.OVER) && (dc.compare( y1, y2 ) > 0))
        || ((mode == PolyLine.UNDER) && (dc.compare( y1, y2 ) < 0)) )
      return true;

    return false;
  }

  /**
   * Returns the range on x
   * 
   * @return
   */
  public Range xExtremum( )
  {
    return new Range( m_x[0], m_x[m_x.length - 1] );
  }

  /**
   * Tells whether the X-range of this polyline lies within the given range:<br>
   * given.range.from &lt;= this.xrange.from &lt;= given.range.to given.range.from &lt;=
   * this.xrange.to &lt;= given.range.to
   * 
   * @param xr
   * @return
   */
  public boolean withinXRange( Range xr )
  {
    return xExtremum().within( xr );
  }

  /**
   * Computes the length of the polyline. The length is the sum of the length of each segment that
   * build this polyline: <br>
   * L = S1 + ... + Si + ... + Sn<br>
   * Si = length of segment i
   * 
   * @return
   */
  public double length( )
  {
    double length = 0;

    for( int i = 0; i < (m_y.length - 1); i++ )
      length += PointUtilities.segmentLength( m_x[i], m_y[i], m_x[i + 1], m_y[i + 1] );

    return length;
  }

  public double[] getX( )
  {
    return m_x;
  }

  public double getFirstX( )
  {
    return m_x[0];
  }

  public double getLastX( )
  {
    return m_x[m_x.length - 1];
  }

  public int getXLength( )
  {
    return m_x.length;
  }

  public double[] getY( )
  {
    return m_y;
  }

  public double getFirstY( )
  {
    return m_y[0];
  }

  public double getLastY( )
  {
    return m_y[m_y.length - 1];
  }

  public int getYLength( )
  {
    return m_y.length;
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    StringBuffer buf = new StringBuffer();

    for( int i = 0; i < m_x.length; i++ )
      buf.append( m_x[i] ).append( ", " ).append( m_y[i] ).append( "\n" );

    return buf.toString();
  }

  /**
   * Returns the delta
   * 
   * @return
   */
  public double getDelta( )
  {
    return m_delta;
  }
}
