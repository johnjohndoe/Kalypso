/**
 * 
 */
package org.kalypso.afgui.model.impl;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;
import org.kalypso.afgui.model.EActivityAction;
import org.kalypso.afgui.model.EActivityExeState;
import org.kalypso.afgui.model.EActivityRelationship;
import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.IActivityRuntimeStatus;
import org.kalypso.afgui.model.IActivitySpecification;
import org.kalypso.afgui.model.IPhase;
import org.kalypso.afgui.model.IRelationshipStatement;
import org.kalypso.afgui.model.ITask;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.afgui.model.IWorkflowRuntineStatus;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.IWorkflowSpecification;
import org.kalypso.afgui.model.events.WorkflowChangeEvent;
import org.kalypso.afgui.model.events.WorkflowChangeEventListerner;
import org.kalypso.afgui.schema.Schema;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.vocabulary.RDF;

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
public class WorkflowImpl implements IWorkflow
{
//	//RuntimeStatus contains the 
//	EActivityAction currentAction;
//	
	final static private Logger logger=
					Logger.getLogger(WorkflowImpl.class);
	/** provided the workflow specification*/
	private IWorkflowSpecification workflowSpecification;
	
	//transition history
	private IWorkflowRuntineStatus runtimeStatus;
	
	private Model model;
	
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
	public WorkflowImpl(
			IWorkflowSpecification workflowSpecification,
			IWorkflowRuntineStatus runtimeStatus,
			Model model)
	{
		if(	workflowSpecification==null || 
			runtimeStatus==null)
		{
			throw new IllegalArgumentException(
					"workflowSpecification and status must not be null");
		}
		this.workflowSpecification=workflowSpecification;
		this.runtimeStatus=runtimeStatus;
		this.wfceListener= new ArrayList<WorkflowChangeEventListerner>();
	}
	
	
	private final IWorkflowSpecification loadSpecifications(URL specURL) throws IOException
	{
		logger.info("specURL="+specURL);
		IWorkflowSpecification wfSpec=null;
		Map<String, IActivitySpecification> specsMap=
			new HashMap<String, IActivitySpecification>();
		List<IRelationshipStatement> stms= new ArrayList<IRelationshipStatement>();
		//model load
		InputStream iStream=specURL.openStream();
		Model rdfModel= ModelFactory.createDefaultModel();
		rdfModel.read(iStream,"");
		rdfModel.write(System.out);
		
		//fill map
		IActivitySpecification spec=null;

		
		StmtIterator it=
			rdfModel.listStatements((Resource)null,RDF.type,Schema.CLASS_ACTIVITY);//selectActivities);
		logger.info("\nHAS_NEXT:"+it.hasNext());
		Statement stm;		
		for(;it.hasNext();)
		{
			stm=it.nextStatement();
			spec= new ActivitySpecification(stm.getSubject());
			logger.info(stm.getPredicate());
			specsMap.put(spec.getID(), spec);
			
		}
		
		//create statement list, get activity link statements, 
		//construct statement based on it
		IActivitySpecification objSpec=null;
		IActivitySpecification subSpec=null;
		IRelationshipStatement relStm=null;
		EActivityRelationship rel;
		
		for(Property prop:Schema.ACTIVITY_LINK_PROPS)
		{
			it=
				rdfModel.listStatements((Resource)null,prop,(Resource)null);
			for(;it.hasNext();)
			{
				stm=it.nextStatement();
				subSpec= specsMap.get(stm.getSubject().getURI());
				objSpec= specsMap.get(((Resource)stm.getObject()).getURI());
				rel=Schema.toEActivityRelationship(prop);
				if(subSpec==null || objSpec==null|| rel==null)
				{
					logger.warn("RelationTripple=["+subSpec+", "+rel+"/"+prop+", "+objSpec);
				}
				else
				{
					relStm= new RelationshipStatement(subSpec,rel,objSpec);
					stms.add(relStm);
					if(rel==EActivityRelationship.PART_OF)
					{
						IRelationshipStatement inverseRelStm=
							new RelationshipStatement(
									objSpec,EActivityRelationship.HAS_A,subSpec);
						if(!stms.contains(inverseRelStm))
						{
							stms.add(inverseRelStm);
						}
					}
					else if(rel==EActivityRelationship.HAS_A)
					{
						IRelationshipStatement inverseRelStm=
							new RelationshipStatement(
									objSpec,EActivityRelationship.PART_OF,subSpec);
						if(!stms.contains(inverseRelStm))
						{
							stms.add(inverseRelStm);
						}
					}
				}
				
			}
		}
		logger.info(stms);
		//create workflow spec
		wfSpec= new WorkflowSpecification(specsMap,stms);
		return wfSpec;
	}

	private final IWorkflowRuntineStatus loadRuntimeStatus(URL specURL)
	{
		IWorkflowRuntineStatus wfStatus=null;
		wfStatus=new WorkflowRuntimeStatus();
		return wfStatus;
	}
	
	
	/**
	 * @see org.kalypso.afgui.model.IWorkflow#getChildrenActivities(org.kalypso.afgui.model.IActivity, org.kalypso.afgui.model.EActivityRelationship)
	 */
	public List<IActivity> getChildrenActivities(
										IActivity activity,
										EActivityRelationship relationship)
	{
		if(activity ==null || relationship==null)
		{
			throw  new IllegalArgumentException("All parameters must not be null");
		}
		List<IActivitySpecification> specs=
			workflowSpecification.getLinkedActivitySpecs(
					activity.getActivitySpecification(), relationship);
		return runtimeStatus.getActivities(specs);
	}

	
	/**
	 * @see org.kalypso.afgui.model.IWorkflow#getRootActivities()
	 */
	public List<IActivity> getRootActivities()
	{
		List<IActivitySpecification> specs=
			workflowSpecification.getRootActivitySpecs();
		return runtimeStatus.getActivities(specs);
	}
	
	@Override
	public String toString()
	{
		StringBuffer buffer = new StringBuffer(256);
		buffer.append("\n==============================");
		buffer.append("\n--->specification\n");
		buffer.append(workflowSpecification.toString());
		buffer.append("\n--->runtime status\n");
		buffer.append(runtimeStatus);
		return buffer.toString();
	}


	public IWorkflowRuntineStatus getRuntineStatus()
	{
		return runtimeStatus;
	}


	public IWorkflowSpecification getWorkflowSpecification()
	{
		return workflowSpecification;
	}


	public IActivity getCurrentActivity()
	{
		return runtimeStatus.getCurrentActivity();
	}


	public void setCurrentActivity(IActivity currentActivity)
	{
		runtimeStatus.setCurrentActivity(currentActivity);
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
	
	/**
	 * @see org.kalypso.afgui.model.IWorkflow#updateWorkflow(org.kalypso.afgui.model.IActivity, org.kalypso.afgui.model.EActivityAction)
	 */
	public void updateWorkflow(IActivity activity, EActivityAction action)
	{
		runtimeStatus.setCurrentActivity(activity);
		runtimeStatus.setCurrentAction(action);
		logger.info("\n*********************************"+
					action +" "+activity);
		if(action==EActivityAction.UP)
		{
			logger.info("\n*********************************"+
					getRootActivities()+" \n"+activity.getName()+ "\n"+
					getRootActivities().contains(activity));
			if(getRootActivities().contains(activity))
			{
				runtimeStatus.setCurrentActivity(null);
			}
			else
			{
			  List<IActivity> as=
				  getChildrenActivities(activity, EActivityRelationship.PART_OF);
			  if(!as.isEmpty())
			  {
				  runtimeStatus.setCurrentActivity(as.get(0));
				  runtimeStatus.setCurrentAction(null);
			  }
			}
		}
		fireWorkflowChangedEvent(new WorkflowChangeEvent(this));
	}
	
	private final void fireWorkflowChangedEvent(WorkflowChangeEvent event)
	{
		for(WorkflowChangeEventListerner l:wfceListener)
		{
			l.onWorkflowChanged(event);
		}
	}
	
	public void adopt(IWorkflow workflow)
	{
		if(workflow==null)
		{
			throw new IllegalArgumentException();
		}
		this.workflowSpecification=workflow.getWorkflowSpecification();
		this.runtimeStatus=workflow.getRuntineStatus();
		fireWorkflowChangedEvent(new WorkflowChangeEvent(this));
	}
	
	public IActivitySpecification getActivitySpecification()
	{
		return null;
	}
	
	public EActivityExeState getExeState()
	{
		return null;
	}
	
	public String getHelp()
	{
		return null;
	}
	
	public String getID()
	{
		return null;
	}
	
	public String getName()
	{
		return null;
	}
	
	public IPhase[] getPhases()
	{
		return null;
	} 
	
	public IActivityRuntimeStatus getRuntimeStatus()
	{
		return null;
	}
	
	public ITask[] getTask()
	{
		return null;
	}
	
	public void setExeState(EActivityExeState status)
	{
		
	}
	public Object getWrappedModel()
	{
		return model;
	}
	
	public IWorkflowData getWorkflowDataModel()
	{
		return null;
	}
}
