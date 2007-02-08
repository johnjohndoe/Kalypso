package org.kalypso.afgui.model.impl;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.Platform;
import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.IPhase;
import org.kalypso.afgui.model.ISubTaskGroup;
import org.kalypso.afgui.model.ITask;
import org.kalypso.afgui.model.ITaskGroup;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.events.WorkflowChangeEventListerner;
import org.kalypso.afgui.schema.Schema;
import com.hp.hpl.jena.rdf.model.Resource;


/**
 * This class abtract the workfow. a workflow is collection of activities.
 * In a workflow activities can be linked through:
 * <ul>
 * 	<li/>a has-a relationship. 
 * 		That is activities may be group in a container activity
 *  <li/>excecution dependency. 
 *  	That is a particular activity can only started if another is done.  
 * <ul/>
 * 
 * The work flow has a root activity, which represent the starting point of for the workflow.
 * The workflow provide the mechanism to navigate throw the the workflow.
 * Particulaly it provides an update() method, 
 * which can be used to update the state of the of the workflow.
 * E.g. transitiing to another activity.
 * 
 *  The Workflow also implements a Event 
 * @author Patrice Congo
 *
 */
/**
 * @author congo
 *
 */
public class WorkflowImpl extends WorkflowPart implements IWorkflow
{
//	final static private Logger logger=
//					Logger.getLogger(WorkflowImpl.class.getName());
//  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) );
//
//  static
//  {
//    if( !log )
//      logger.setUseParentHandlers( false );
//  }
  
	
	final private List<WorkflowChangeEventListerner> wfceListener;
	
	
	/**
	 * Construct a new Workflow specification based on the given
	 * workflow specification
	 * @param workflowSpecification  the work flow specification this 
	 *     workflow is based on. this parameter must not be null
	 * @param runtimeStatus the runtime status for this workflow
	 * @throws IllegalArgumentException if the parameter workflowSpecification is  null
	 * 			
	 */
	public WorkflowImpl(Resource resource)
	{
		super(resource);
		this.wfceListener= new ArrayList<WorkflowChangeEventListerner>();
	}
	
	
	

	@Override
	public String toString()
	{
		StringBuffer buffer = new StringBuffer("Workflow.");
		buffer.append(getName());
		return buffer.toString();
	}



	public void addWorkflowChangedEventListener(WorkflowChangeEventListerner l)
	{
		if(l==null)
		{
			throw new IllegalArgumentException(
							"Listener must not be null");
		}
		if(!wfceListener.contains(l))
		{
			wfceListener.add(l);
		}
	}


	public void removeWorkflowChangedEventListener(WorkflowChangeEventListerner l)
	{
		if(l==null)
		{
			throw new IllegalArgumentException(
					"listener must not be null");
		}
		wfceListener.remove(l);
	}
	
//	/**
//	 * @see org.kalypso.afgui.model.IWorkflow#updateWorkflow(org.kalypso.afgui.model.IActivity, org.kalypso.afgui.model.EActivityAction)
//	 */
//	public void updateWorkflow(IActivity activity, EActivityAction action)
//	{
//		runtimeStatus.setCurrentActivity(activity);
//		runtimeStatus.setCurrentAction(action);
//		logger.info("\n*********************************"+
//					action +" "+activity);
//		if(action==EActivityAction.UP)
//		{
//			logger.info("\n*********************************"+
//					getRootActivities()+" \n"+activity.getName()+ "\n"+
//					getRootActivities().contains(activity));
//			if(getRootActivities().contains(activity))
//			{
//				runtimeStatus.setCurrentActivity(null);
//			}
//			else
//			{
//			  List<IActivity> as=
//				  getChildrenActivities(activity, EActivityRelationship.PART_OF);
//			  if(!as.isEmpty())
//			  {
//				  runtimeStatus.setCurrentActivity(as.get(0));
//				  runtimeStatus.setCurrentAction(null);
//			  }
//			}
//		}
//		fireWorkflowChangedEvent(new WorkflowChangeEvent(this));
//	}
	
//	private final void fireWorkflowChangedEvent(WorkflowChangeEvent event)
//	{
//		for(WorkflowChangeEventListerner l:wfceListener)
//		{
//			l.onWorkflowChanged(event);
//		}
//	}




	/**
	 * @see org.kalypso.afgui.model.IWorkflow#getPhases()
	 */
	public List<IPhase> getPhases()
	{
		return Schema.getPhase(resource);
	}




	/**
	 * @see org.kalypso.afgui.model.IPhase#getTaskGroups()
	 */
	public List<ITaskGroup> getTaskGroups()
	{
		return Schema.getTaskGroups(resource);
	}




	/**
	 * @see org.kalypso.afgui.model.ITaskGroup#getSubTaskGroups()
	 */
	public List<ISubTaskGroup> getSubTaskGroups()
	{
		return Schema.getSubTaskGroups(resource);
	}




	/**
	 * @see org.kalypso.afgui.model.ISubTaskGroup#getTasks()
	 */
	public List<ITask> getTasks()
	{
		return Schema.getTasks(resource);
	}




	/**
	 * @see org.kalypso.afgui.model.ITask#getActivities()
	 */
	public List<IActivity> getActivities()
	{
		return Schema.getActivities(resource);
	}
	
	
}
