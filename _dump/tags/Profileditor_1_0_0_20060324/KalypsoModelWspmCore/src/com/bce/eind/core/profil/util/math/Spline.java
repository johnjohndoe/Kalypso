package com.bce.eind.core.profil.util.math;

/**
 * Represents a Spline. Interpolation can be performed using eval().<br>
 * Algorithm is adapted from Book: Algorithmen, Robert Sedgewick (3-8273-7032-9) 
 *
 * @author schlienger
 */
public class Spline
{
	/** x coordinates */
    private double[] m_x;
    
    /** y coordinates */
    private double[] m_y;
    
    /** dient zur Darstellung der tridiagonalen Matrix */
    private double[] m_u;
    
    /** */
    private double[] m_p;
    
    /** used internally to check the state of this spline object */
    private boolean m_initDone;

    /**
     * Default Constructor
     */
    public Spline(  )
    {
        m_initDone = false;
    }

    /**
     * Constructor with coordinates
     *
     * @param x -
     * @param y -
     */
    public Spline( double[] x, double[] y )
    {
        setPoints( x, y );
    }

    /**
     * Sets the coordinates of the spline and constructs it.
     *
     * @param x -
     * @param y -
     */
    public void setPoints( double[] x, double[] y )
    {
        m_x = x;
        m_y = y;

        makeSpline(  );

        m_initDone = true;
    }

    public double eval( double value )
    {
        if( !m_initDone )
            throw new IllegalStateException( "Spline wurde nicht initialisiert" );

        int i = -1;

        do
            i++;
        while( value > m_x[i + 1] );

        double t = ( value - m_x[i] ) / m_u[i];

        return ( t * m_y[i + 1] ) + ( ( 1 - t ) * m_y[i] ) +
        ( ( m_u[i] * m_u[i] * ( ( f( t ) * m_p[i + 1] ) + ( f( 1 - t ) * m_p[i] ) ) ) / 6.0 );
    }

    private double f( double d )
    {
        return ( d * d * d ) - d;
    }

    /**
     * Constructs the inernal state of the spline
     */
    private void makeSpline(  )
    {
        int N = m_x.length;

        double[] d = new double[N - 1];

        for( int i = 1; i < ( N - 1 ); i++ )
            d[i] = 2 * ( m_x[i + 1] - m_x[i - 1] );

        m_u = new double[N - 1];

        for( int i = 0; i < ( N - 1 ); i++ )
            m_u[i] = m_x[i + 1] - m_x[i];

        double[] w = new double[N - 1];

        for( int i = 1; i < ( N - 1 ); i++ )
            w[i] =
                6.0 * ( ( ( m_y[i + 1] - m_y[i] ) / m_u[i] ) -
                ( ( m_y[i] - m_y[i - 1] ) / m_u[i - 1] ) );

        m_p = new double[N];
        m_p[0] = 0.0;
        m_p[N - 1] = 0.0;

        for( int i = 1; i < ( N - 2 ); i++ )
        {
            w[i + 1] = w[i + 1] - ( ( w[i] * m_u[i] ) / d[i] );
            d[i + 1] = d[i + 1] - ( ( m_u[i] * m_u[i] ) / d[i] );
        }

        for( int i = N - 2; i >= 1; i-- )
            m_p[i] = ( w[i] - ( m_u[i] * m_p[i + 1] ) ) / d[i];
    }
}
