/*
 * Created on 22.04.2003
 */
package com.bce.eind.core.profil.util.geom;

import java.awt.geom.Line2D;
import java.awt.geom.Point2D;

import com.bce.eind.core.profil.util.math.LinearEquation;
import com.bce.eind.core.profil.util.math.Utilities;
import com.bce.eind.core.profil.util.math.LinearEquation.SameXValuesException;


/**
 * Trapeze.
 *
 * @author schlienger
 */
public class Trapeze
{
    private Point2D m_p11 = null;
    private Point2D m_p12 = null;
    private Point2D m_p21 = null;
    private Point2D m_p22 = null;

    /**
     * "Empty" Trapeze.  Further operations like area() will throw an
     * IllegalStateException as long as you do not call either setPoints() or setLines()
     */
    public Trapeze(  )
    {
    }

    /**
     * Constructs a trapeze with two parallel lines.
     *
     * @param L1
     * @param L2
     *
     * @see setLines(Line2D L1, Line2D L2)
     */
    public Trapeze( Line2D L1, Line2D L2 )
    {
        setLines( L1, L2 );
    }

    /**
     * Points p11 and p12 must represent a line L1. Points p21 and p22 must represent a
     * line L2.  L1 must be parallel to L2.
     *
     * @param p11
     * @param p12
     * @param p21
     * @param p22
     *
     * @see setPoints(Point2D p11, Point2D p12, Point2D p21, Point2D p22)
     */
    public Trapeze( Point2D p11, Point2D p12, Point2D p21, Point2D p22 )
    {
        setPoints( p11, p12, p21, p22 );
    }

    /**
     * Lines L1 and L2 must be parallel if you want meaningful results !!!
     *
     * @param L1
     * @param L2
     */
    public void setLines( Line2D L1, Line2D L2 )
    {
        m_p11 = L1.getP1(  );
        m_p12 = L1.getP2(  );
        m_p21 = L2.getP1(  );
        m_p22 = L2.getP2(  );
    }

    /**
     * Points p11 and p12 must represent a line L1. Points p21 and p22 must represent a
     * line L2.  L1 must be parallel to L2.
     *
     * @param p11
     * @param p12
     * @param p21
     * @param p22
     */
    public void setPoints( Point2D p11, Point2D p12, Point2D p21, Point2D p22 )
    {
        m_p11 = p11;
        m_p12 = p12;
        m_p21 = p21;
        m_p22 = p22;
    }

    /**
     * Returns the height ofthe trapeze.
     *
     * @return height of the trapeze, that is the distance between the two bases.
     *
     * @throws IllegalStateException if points of trapeze are null
     */
    public double height(  )
    {
        if( ( m_p11 == null ) || ( m_p21 == null ) )
            throw new IllegalStateException( 
                "Trapeze has not a valid state: points are null." );

        if( m_p11.getX(  ) == m_p12.getX(  ) )
            return Math.abs( m_p11.getX(  ) - m_p21.getX(  ) );
        else
        {
            try
            {
                LinearEquation eq = new LinearEquation( m_p11, m_p12 );
                
				return eq.distance( m_p21 );
            }
            catch( SameXValuesException e )
            {
            	// sollte nicht der fall sein weil wir der test explizit vorher machen
                return 0;               
            }
        }
    }

    /**
     *
     * @return area of the trapeze, according to the formula: A = (b + B)  H / 2
     *
     * @throws IllegalStateException -
     */
    public double area(  )
    {
        if( ( m_p11 == null ) || ( m_p12 == null ) || ( m_p21 == null ) ||
                ( m_p22 == null ) )
            throw new IllegalStateException( 
                "Trapeze has not a valid state: points are null." );

        double d1 = Utilities.distance( m_p11, m_p12 );
        double d2 = Utilities.distance( m_p21, m_p22 );
        double h = height(  );

        return ( ( d1 + d2 ) * h ) / 2;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    public String toString(  )
    {
        StringBuffer buf = new StringBuffer(  );

        buf.append( m_p11 + "|" + m_p12 + "\n" );
        buf.append( "-------------------------------------------------\n" );
        buf.append( m_p21 + "|" + m_p22 + "\n" );

        return buf.toString(  );
    }
}
