/**
 * 
 */
package org.kalypso.afgui.model.impl;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Hashtable;
import java.util.Map;

import org.apache.log4j.Logger;
import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.IWorkflowPartRTContext;
import org.kalypso.afgui.model.IWorkflowSystem;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;


/**
 * 
 * @author Patrice Congo
 */
public class WorkflowSystem implements IWorkflowSystem
{
	final static private Logger logger=
				Logger.getLogger(WorkflowSystem.class);
	
	final private Model specModel;
	
	final private Model mergedModel;
	
	private IWorkflowPartRTContext currentWorkflowRTContext;
	private IWorkflowPartRTContext currentTaskGroupRTContext;
	private IWorkflowPartRTContext currentSubTaskGroupRTContext;
	private IWorkflowPartRTContext currentTaskRTContext;
	//private IWorkflowPartRTContext currentActivityRTContext;
	
	private IWorkflowData currentDataModel;
	
	private IWorkflow currentWorkflow;
	
	public Map<String, IActivity> activities= new Hashtable<String, IActivity>();
	
	
	public WorkflowSystem(URL specURL, URL statusURL) throws IOException
	{
		specModel=loadModel(specURL);
		mergedModel=loadModel(statusURL);
		mergedModel.add(specModel);
		//mergedModel.difference(arg0)
	}
	
	private final Model loadModel(URL url) throws IOException
	{
		logger.info("specURL="+url);
		InputStream iStream=url.openStream();
		Model rdfModel= ModelFactory.createDefaultModel();
		rdfModel.read(iStream,"");
		//rdfModel.write(System.out);
		return rdfModel;
	}
	
//	public IWorkflow createWorkflow(URL specURL, URL statusURL) throws IOException
//	{
//		if(specURL==null)
//		{
//			throw new IllegalArgumentException(
//					"A spec url is needed to create a workspace");
//		}
//		
//		return new WorkflowImpl(
//				loadSpecifications(specURL),
//				loadRuntimeStatus(statusURL)); 
//	}
	
	/**
	 * To get the workflow for the given resource.
	 * If their is a worflow in the system for this resource
	 * then it is returned otherwise a new one is created 
	 * 
	 * @param resouceType
	 * @return
	 */
	public IWorkflow getWorflow(IWorkflowData dataModel)
	{
		return null;
	}

	public IWorkflow getCurrentWorkFlow()
	{
		return null;
	}

	public IWorkflow setCurrent(IWorkflowData dataModel)
	{
		return null;
	}
	
	
	public IActivity getActivity(String uri)
	{
		if(uri==null)
		{
			return null;
		}
		else
		{
			return activities.get(uri);
		}
	}

	public void updateRTContext(IWorkflowPartRTContext currentWorkflowRTContext, IWorkflowPartRTContext currentTaskGroupRTContext, IWorkflowPartRTContext currentSubTaskGroupRTContext, IWorkflowPartRTContext currentTaskRTContext)
	{
		// TODO Auto-generated method stub
		
	}

	public void updateRTContext(IWorkflowPartRTContext currentTaskGroupRTContext, IWorkflowPartRTContext currentSubTaskGroupRTContext, IWorkflowPartRTContext currentTaskRTContext)
	{
		// TODO Auto-generated method stub
		
	}

	public void updateRTContext(IWorkflowPartRTContext currentSubTaskGroupRTContext, IWorkflowPartRTContext currentTaskRTContext)
	{
		// TODO Auto-generated method stub
		
	}

	public void updateRTContext(IWorkflowPartRTContext currentTaskRTContext)
	{
		// TODO Auto-generated method stub
		
	}
	
}
