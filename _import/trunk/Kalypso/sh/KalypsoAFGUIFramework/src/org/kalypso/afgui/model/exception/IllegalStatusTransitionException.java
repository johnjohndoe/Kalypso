/**
 * 
 */
package org.kalypso.afgui.model.exception;

import org.eclipse.ui.activities.IActivity;
import org.kalypso.afgui.model.EActivityExeState;

/**
 * Exception is thrown to signal that a status transtion for
 * an activity is illegal
 * 
 * @author Patrice Congo
 *
 */
public class IllegalStatusTransitionException extends Exception
{
	/**
	 * The activity which statuc was to be changed 
	 */
	final private IActivity activity;
	
	/**
	 * The currentStatus of the activity
	 */
	final private EActivityExeState currentStatus;
	
	/**
	 * The intented nextStatus of the activity
	 */
	final private EActivityExeState nextStatus;
	
	/**
	 * @param message
	 */
	public IllegalStatusTransitionException(
						EActivityExeState currentStatus,
						EActivityExeState nextStatus,
						IActivity activity,
						String message)
	{
		super(message,null);
		this.activity=activity;
		this.currentStatus=currentStatus;
		this.nextStatus=nextStatus;
	}

	/**
	 * Return the activity which status modification attempt triggers this exception
	 * @return
	 */
	public IActivity getActivity()
	{
		return activity;
	}

	/**
	 * 
	 * @return
	 */
	public EActivityExeState getCurrentStatus()
	{
		return currentStatus;
	}

	public EActivityExeState getNextStatus()
	{
		return nextStatus;
	}

	
	

}
