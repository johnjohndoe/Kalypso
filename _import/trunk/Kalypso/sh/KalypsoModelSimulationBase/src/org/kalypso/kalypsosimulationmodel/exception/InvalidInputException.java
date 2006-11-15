package org.kalypso.kalypsosimulationmodel.exception;

/**
 * Exception that signal any invalid input
 * 
 * @author Patrice Congo
 *
 */
public class InvalidInputException extends RuntimeException
{

	/**
	 * Create an empty invalid Input exception
	 */
	public InvalidInputException()
	{
			//empty
	}

	/**
	 * Create an invalid input with the given message
	 * @param message
	 */
	public InvalidInputException(String message)
	{
		super(message);
	}

}
