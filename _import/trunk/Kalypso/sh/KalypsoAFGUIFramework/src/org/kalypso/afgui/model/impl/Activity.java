/**
 * 
 */
package org.kalypso.afgui.model.impl;


import org.kalypso.afgui.model.EActivityExeState;
import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.IActivityRuntimeStatus;
import org.kalypso.afgui.model.IActivitySpecification;


/**
 * Provide the default implementation of {@link IActivity} 
 * @author Patrice Congo
 */
public class Activity implements IActivity
{
	/**
	 * The specification of the activity
	 */
	final private IActivitySpecification spec;
	
	/**
	 * the current status of the activity
	 */
	private IActivityRuntimeStatus status;
	
	/**
	 * Create an Activity base on the provided specification and having 
	 * the given status
	 * 
	 * @param spec -- the spec of the activity to create
	 * @param status -- the status of the activity to create
	 */
	public Activity(
				IActivitySpecification spec, 
				IActivityRuntimeStatus status)
	{
		if(spec==null)
		{
			throw new IllegalArgumentException("Param spec must not be null");
		}
		
		this.spec=spec;
		this.status=status;
	}
	
//	/**
//	 * Creates an activity based on the provided specification. 
//	 * The status of the created Activity is set to {@link EActivityExeState#NOT_STARTED}
//	 * 
//	 * @param spec -- the specification of the activity
//	 */
//	public Activity(IActivitySpecification spec)
//	{
//		this(spec,EActivityExeState.NOT_STARTED);
//	}
	
	
	/**
	 * @see org.kalypso.afgui.model.IActivity#getActivitySpecification()
	 */
	public IActivitySpecification getActivitySpecification()
	{
		return spec;
	}

	/**
	 * @see org.kalypso.afgui.model.IActivity#getID()
	 */
	public String getID()
	{
		return spec.getID();
	}
	
	public String getName()
	{
		return spec.getName();
	}

	/**
	 * @see org.kalypso.afgui.model.IActivity#getStatus()
	 */
	public EActivityExeState getExeState()
	{
		return status.getExeState();
	}

	/**
	 * Sets the status of this activity.
	 * no validation of transition is made. It is thus to callers responsibility 
	 * to make the check.
	 * 
	 * @see org.kalypso.afgui.model.IActivity#setStatus(org.kalypso.afgui.model.EActivityExeState)
	 */
	public void setExeState(
			EActivityExeState exeState)	
	{
		status.setExeState(exeState);		
	}
	
	public IActivityRuntimeStatus getRuntimeStatus()
	{
		return status;
	}
	@Override
	public boolean equals(Object obj)
	{
		if(obj==null)
		{
			return false;
		}
		else if(this==obj)
		{
			return true;
		}
		else if(obj instanceof Activity)
		{
			try
			{
				return getID().equals(((Activity)obj).getID());
			}
			catch(Throwable th)
			{
				th.printStackTrace();
				return false;
			}
		}
		else
		{
			return false;
		}
	}
	
	@Override
	public String toString()
	{
		return getName();
	}

	public String getHelp()
	{
		return  spec.getHelp().getHelp();
	}
}
