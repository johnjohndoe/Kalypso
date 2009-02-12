
package de.tuhh.kalypso.util.ErrorHandler;
/*
 * @(#)KalypsoFilterException.java	1.0 01/12/03
 *
 */


/**
 * Signals that an Kalypso Filter exception of some sort has occurred. This
 * class is the general class of exceptions produced by failed or
 * interrupted logical filter operations.
 *
 * @author  C.Kuepferle
 * @version 1.0, 08/01/03
 */
public class KalypsoFilterException extends Exception
{
	public final static int KF_UNKNOWN = -1;
	public int etype = KF_UNKNOWN;
	public final static int CIRCULAR_REFFERENCE = -2;
	public final static int INVALID_ELEMENT = -3;
	public final static int FILE_FORMAT_ERROR = -4;
	public final static int NET_ERROR = -5;
	
	/**
	 * Constructs an <code>KalypsoFilterException</code> with <code>null</code>
	 * as its error detail message.
	 */
	public  KalypsoFilterException( String message )
	{
		super( message );
	}
	
	/**
	 * Constructs an <code>KalypsoFilterException</code> with the specified detail
	 * message. The error message string <code>s</code> can later be
	 * retrieved by the <code>{@link java.lang.Throwable#getMessage}</code>
	 * method of class <code>java.lang.Throwable</code>.
	 *
	 * @param   s   the detail message.
	 */
	public KalypsoFilterException( String message, int type )
	{
		super( message );
		etype = type;
	}
	public int getExceptionType() { return etype; }
}

