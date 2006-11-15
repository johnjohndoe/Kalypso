package org.kalypso.kalypsosimulationmodel.exception;

/**
 * This class abtracts exceptions caused by an invalid index value.
 * The invalidity means out of range. The range information may be passed
 * by at construction.
 *  
 * @author Patrice Congo
 */
public class IndexOutOfRangeException extends IndexOutOfBoundsException
{
	/**
	 * The invalid index
	 */
	final private int index;

	/**
	 * The minimal valid  index value
	 */
	final private int rangeMin;
	
	/**
	 * the maximal valid index value
	 */
	final private int rangeMax;
	
	/**
	 * Create an IndexOutOfRange Exception cause by the given index for 
	 * the specified class 
	 */
	public IndexOutOfRangeException(int index, int rangeMin, int rangeMax)
	{
		this(null, index, rangeMin, rangeMax);
	}

	/**
	 * @param message
	 */
	public IndexOutOfRangeException(String message)
	{
		this(message,-1,-1,-1);
	}

	/**
	 * 
	 * @param message
	 * @param cause -- the cause of
	 * @param index the invalid index value
	 * @param boundMin the minimal valid index value
	 * @param boundMax  the maximal valid index value
	 * 
	 */
	public IndexOutOfRangeException(
						String message, 
						int index,
						int rangeMin,
						int rangeMax)
	{
		super(message);
		this.rangeMax=rangeMax;
		this.rangeMin=rangeMin;
		this.index=index;
	}

	/**
	 * The invalid index, that causes the exception
	 * @return the index
	 */
	public int getIndex()
	{
		return index;
	}

	/**
	 * Returns the maximal valid value for the index
	 * 
	 * @return the rangeMax
	 */
	public int getRangeMax()
	{
		return rangeMax;
	}

	/**
	 * returns the minimal valid value for the index
	 * @return the rangeMin
	 */
	public int getRangeMin()
	{
		return rangeMin;
	}

	
}
