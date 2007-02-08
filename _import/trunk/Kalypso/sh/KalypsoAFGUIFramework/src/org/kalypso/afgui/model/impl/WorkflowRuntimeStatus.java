/**
 * 
 */
package org.kalypso.afgui.model.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.eclipse.core.runtime.Platform;
import org.kalypso.afgui.model.EActivityAction;
import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.IActivityRuntimeStatus;
import org.kalypso.afgui.model.IActivitySpecification;
import org.kalypso.afgui.model.IWorkflowRuntineStatus;

/**
 * This class is used to describe the current runtime status of a workflow.
 * This runtime static is of activities which have been already startet.
 * 
 * @author Patrice Congo
 *
 */
public class WorkflowRuntimeStatus implements IWorkflowRuntineStatus
{
	final static private Logger logger=
			Logger.getLogger(WorkflowSpecification.class.getName());
    private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) );

    static
    {
      if( !log )
        logger.setUseParentHandlers( false );
    }
    
    /**
	 * A map containing the instantiated activities 
	 */
	private final Map<String , IActivity> activities;
	
	private IActivity currentActivity;
	private EActivityAction currentAction;
	
	public WorkflowRuntimeStatus()
	{
		activities= new HashMap<String, IActivity>(64);
		currentActivity=null;
	}
	
	public WorkflowRuntimeStatus(
			IActivity currentActivity,
			Map<String,IActivityRuntimeStatus> exeStates,			
			Map<String, IActivitySpecification> specs
						)
	{
		if(exeStates==null || specs==null)
		{
			throw new IllegalArgumentException(
					"Argument activities must not be null"+
					"\nexeState="+exeStates+
					"\nspecs="+specs+
					"currentActivity="+currentActivity);
		}
		this.currentActivity=currentActivity;
		
		//build the activity table,
		this.activities= new HashMap<String, IActivity>((int)(exeStates.size()*1.5));
		String actName=null;
		IActivityRuntimeStatus status;
		
		for(Map.Entry<String, IActivitySpecification> spec : specs.entrySet())
		{
			actName=spec.getKey();
			status=exeStates.get(actName);
			if(status!=null)
			{
				status= new ActivityRuntimeStatus();
			}
//			activities.put(actName, new Activity(spec.getValue(),status));
		}
	}
	
	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IWorflowRuntineStatus#getActivities(java.util.List)
	 */
	public List<IActivity> getActivities(
					List<IActivitySpecification> specifications)
					throws IllegalArgumentException
	{
		if(specifications== null)
		{
			throw new IllegalArgumentException("Argument specifications must not be null");
		}
		if(specifications.isEmpty())
		{
			return Collections.emptyList();
		}
		else
		{
			final int SIZE=specifications.size();
			logger.info("\nSIZE="+SIZE);
			List<IActivity> activities= new ArrayList<IActivity>(SIZE);
			IActivitySpecification curSpec=null;
			
			for(int index=0;index<SIZE;index++)
			{
				curSpec=specifications.get(index);
				activities.add(
						findOrCreate(curSpec));
			}
			return activities;
		}
	}
	
	/**
	 * Utility method that return an activity corresponding to a cache
	 * @param spec
	 * @return
	 */
	private final IActivity findOrCreate(IActivitySpecification spec)
	{
		final String name=spec.getID();
		IActivity a=activities.get(name);
		if(a==null)
		{
//			a=new Activity(spec,null);
		}
		return a;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IWorflowRuntineStatus#getCurrentActivity()
	 */
	public IActivity getCurrentActivity()
	{
		return currentActivity;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IWorflowRuntineStatus#setCurrentActivity(org.kalypso.afgui.model.IActivity)
	 */
	public void setCurrentActivity(IActivity currentActivity)
	{
		this.currentActivity = currentActivity;
	}
	
	public void setCurrentAction(EActivityAction action)
	{
		this.currentAction=action;
	}
	 
	@Override
	public String toString()
	{
		StringBuffer buf= new StringBuffer(512);
		buf.append("WF_RT[");
		buf.append(activities);
		buf.append(" CA=");
		buf.append(currentActivity);
		buf.append(" A=");
		buf.append(currentAction);
		buf.append(" ]");
		return buf.toString();
	}
	
	public EActivityAction getCurrentAction()
	{
		return currentAction;
	}
}
