package com.bce.eind.core.profil.util;

import gnu.trove.TDoubleArrayList;

import java.text.NumberFormat;


/**
 * holds a range (from-to) and an optional precision with optional factor.
 *
 * @author schlienger
 */
public class Range
{
	/**
	 * unit factor if precision has another unit than from or to.<br>
	 * NOTE: the result of (m_factor x m_precision) has the same unit has from or to.
	 * Default value is 1.
	 */
	private double m_factor = 1;

	/** range from */
	private double m_from;

	/**
	 * precision, value to add/substract to/from range's values to step forwards/backwards.<br>
	 * Default vlaue is 1.
	 */
	private double m_precision = 1;

	/** range to */
	private double m_to;

	/**
	 * constructor with default values
	 *
	 * @param from
	 * @param to
	 */
	public Range( double from, double to )
	{
		m_from = from;
		m_to = to;
	}

	/**
	 * -
	 *
	 * @param from -
	 * @param to -
	 * @param precision -
	 */
	public Range( double from, double to, double precision )
	{
		this( from, to );

		m_precision = precision;
	}

	/**
	 * Constructor
	 *
	 * @param from
	 * @param to
	 * @param precision
	 * @param factor
	 */
	public Range( double from, double to, double precision, double factor )
	{
		this( from, to );

		m_precision = precision;
		m_factor = factor;
	}

	/**
	 * Constructor with Range
	 *
	 * @param r
	 */
	public Range( Range r )
	{
		this( r.m_from, r.m_to, r.m_precision, r.m_factor );
	}

	/**
	 * Returns all the values from this range, begining with range.from, and incrementing
	 * by step up/down to range.to
	 *
	 * @return array of doubles
	 *
	 * @throws IllegalStateException if step == 0
	 */
	public double[] getArray(  ) throws IllegalStateException
	{
		double step = m_precision * m_factor;

		if( Double.compare( step, 0 ) == 0 )
			throw new IllegalStateException( "Step = 0" );

		// estimate initial size
		int count = (int) Math.round( Math.abs( m_to - m_from ) / step );

		TDoubleArrayList values = new TDoubleArrayList(count);
		
		int sign = 1;

		if( m_from > m_to )
			sign = -1;

		step *= sign;

		// create values that span the range
		for( double d = m_from;
				( Double.compare( m_to, d ) == sign ) ||
				( Double.compare( m_to, d ) == 0 ); d += step )
			values.add(d);

		return values.toNativeArray();
	}

	/**
	 * Sets the conversion factor to bring the precision at the same unit as from, to.
	 *
	 * @param d
	 *
	 * @throws IllegalArgumentException if factor == 0
	 */
	public void setFactor( double d )
	{
		if( Double.compare( d, 0 ) == 0 )
			throw new IllegalArgumentException( "factor = 0" );

		m_factor = d;
	}

	public double getFactor(  )
	{
		return m_factor;
	}

	public void setFrom( double d )
	{
		m_from = d;
	}

	public double getFrom(  )
	{
		return m_from;
	}

	public void setPrecision( double d )
	{
		m_precision = d;
	}

	public double getPrecision(  )
	{
		return m_precision;
	}

	public void setTo( double d )
	{
		m_to = d;
	}

	public double getTo(  )
	{
		return m_to;
	}

	/**
	 * Returns a string that describes the contents of this Range<br>
	 * Example: [1, 10] precision = 1, factor= 1 will give:<br>
	 * [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
	 *
	 * @return -
	 */
	public String dump(  )
	{
		StringBuffer buf = new StringBuffer(  );

		buf.append( toString(  ) ).append( "\t[" );

		double[] ds = getArray(  );

		for( int i = 0; i < ( ds.length - 1 ); i++ )
			buf.append( ds[i] ).append( ", " );

		buf.append( ds[ds.length - 1] ).append( "]" );

		return buf.toString(  );
	}

	/**
	 * Merges both ranges so that the newly created one is as follows:
	 * <pre>
	 * r.from = r1.from &lt; r2.from ? r2.from : r1.from
	 * r.to   = r1.to   &gt; r2.to   ? r2.to   : r1.to 
	 * </pre>
	 * 
	 * <p>
	 * Parameter Ranges can be null:<br>
	 * - one of them is null, then a new range copyied from the non null param is
	 * returned - if both Ranges are null, this method returns null. <br>
	 * If Ranges do not intersect, then merge is NOT possible and therefore it  throws
	 * an IllegalArgumentException.
	 * </p>
	 *
	 * @param r1 [can be null]
	 * @param r2 [can be null]
	 *
	 * @return the newly created Range or null if both args are null
	 *
	 * @throws IllegalArgumentException when ranges do not intersect
	 */
	public static Range mergeShort( Range r1, Range r2 )
	{
		if( ( r1 != null ) && ( r2 != null ) )
		{
			if( r1.m_to < r2.m_from )
				throw new IllegalArgumentException( "Ranges do not intersect" );

			double from = ( r1.m_from < r2.m_from ) ? r2.m_from : r1.m_from;
			double to = ( r1.m_to > r2.m_to ) ? r2.m_to : r1.m_to;

			return new Range( from, to, r1.m_precision, r1.m_factor );
		}
		else
		{
			if( r1 != null )
				return new Range( r1 );
			else if( r2 != null )
				return new Range( r2 );
			else

				return null;
		}
	}

	/**
	 * Merges both ranges so that the newly created one is as follows:
	 * <pre>
	 * r.from = r1.from &lt; r2.from ? r1.from : r2.from
	 * r.to   = r1.to   &gt; r2.to   ? r1.to   : r2.to 
	 * </pre>
	 * 
	 * <p>
	 * Parameter Ranges can be null:<br>
	 * - one of them is null, then a new range copyied from the non null param is
	 * returned - if both Ranges are null, this method returns null.
	 * </p>
	 *
	 * @param r1 [can be null]
	 * @param r2 [can be null]
	 *
	 * @return the newly created Range
	 */
	public static Range mergeWide( Range r1, Range r2 )
	{
		if( ( r1 != null ) && ( r2 != null ) )
		{
			double from = ( r1.m_from < r2.m_from ) ? r1.m_from : r2.m_from;
			double to = ( r1.m_to > r2.m_to ) ? r1.m_to : r2.m_to;

			return new Range( from, to, r1.m_precision, r1.m_factor );
		}
		else
		{
			if( r1 != null )
				return new Range( r1 );
			else if( r2 != null )
				return new Range( r2 );
			else

				return null;
		}
	}

	/**
	 * Returns true if this range contains the given value, that is if:<br>
	 * range.from &lt;= value &lt;= range.to
	 *
	 * @param value
	 *
	 * @return
	 */
	public boolean contains( double value )
	{
		if( ( Double.compare( m_from, value ) <= 0 ) &&
				( Double.compare( value, m_to ) <= 0 ) )
			return true;
		else

			return false;
	}

	/**
	 * Returns the length of the Range, as Math.abs( to - from )
	 *
	 * @return
	 */
	public double length(  )
	{
		return Math.abs( m_to - m_from );
	}

	/**
	 * Returns true if ranges are the same:<br>
	 * - same from value<br>
	 * - same to value<br>
	 * - same precision<br>
	 * - same factor<br>
	 * 
	 * <p>
	 * The Double.compare() method is used to perform the comparison.
	 * </p>
	 *
	 * @param other the other range to compare to
	 *
	 * @return ?
	 */
	public boolean same( Range other )
	{
		if( other != null )
		{
			if( ( Double.compare( m_from, other.m_from ) == 0 ) &&
					( Double.compare( m_to, other.m_to ) == 0 ) &&
					( Double.compare( m_precision, other.m_precision ) == 0 ) &&
					( Double.compare( m_factor, other.m_factor ) == 0 ) )
				return true;
			else

				return false;
		}
		else

			return false;
	}

	/**
	 * Returns a string representation of the range in the form: [from , to]
	 *
	 * @return -
	 */
	public String toString(  )
	{
		NumberFormat f = NumberFormat.getInstance(  );

		return f.format( m_from ) + " - " + f.format( m_to );
	}

	/**
	 * Returns true if this range lies within the given range. That is, the from, to
	 * value of this range are between the from, to values of the given range:<br>
	 * other.from &lt;= this.from &lt;= other.to<br>
	 * and<br>
	 * other.from &lt;= this.to &lt;= other.to
	 * 
	 * <p>
	 * The Double.compare() method is perform the comparisons.
	 * </p>
	 *
	 * @param other
	 *
	 * @return
	 */
	public boolean within( Range other )
	{
		if( ( Double.compare( other.m_from, m_from ) <= 0 ) &&
				( Double.compare( m_from, other.m_to ) <= 0 ) &&
				( Double.compare( other.m_from, m_to ) <= 0 ) &&
				( Double.compare( m_to, other.m_to ) <= 0 ) )
			return true;
		else

			return false;
	}
}
