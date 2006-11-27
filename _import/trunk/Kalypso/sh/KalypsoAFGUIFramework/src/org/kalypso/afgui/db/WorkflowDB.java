package org.kalypso.afgui.db;

import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.afgui.schema.Schema;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.ResIterator;

public class WorkflowDB implements IWorkflowDB
{
	
	interface IWorkflowDataCreationMechanism
	{
		public IWorkflowData create(
								Object model,
								String newID,
								String type,
								IWorkflowData parent);
	}
	
	final private Map< String, IWorkflowDataCreationMechanism> 
				creationMechanism= new Hashtable<String, IWorkflowDataCreationMechanism>();
	
	private Model workFlowrdfDBModel;
	
	public IWorkflowData createWorkflowData(
										String id, 
										String type,
										IWorkflowData parent)
	{
		if(type==null)
		{
			throw new IllegalArgumentException(
					"Arguments type must not be null: id="+
					id+" type="+type+" parent="+parent);
		}
		IWorkflowDataCreationMechanism creator=
								creationMechanism.get(type);
		
		return creator.create(workFlowrdfDBModel, id, type, parent);
	}

	public IWorkflowData derivedWorkflowData(
									IWorkflowData parent, 
									String childId)
	{
		return null;
	}

	public IWorkflowData getWorkflowDataById(String id)
	{
		return Schema.getWorkflowDataById(workFlowrdfDBModel, id);
	}

	public List<IWorkflowData> getWorkflowDataByType(String type)
	{
		return Schema.getWorkflowDataByType(workFlowrdfDBModel, type);
	}
	
	

	public void link(
				IWorkflowData subject, 
				IWorkflowData object, 
				EWorkflowProperty prop)
	{
		Schema.createStatement(workFlowrdfDBModel, subject, object, prop);
	}

	public void unlink(IWorkflowData subject, IWorkflowData object, EWorkflowProperty prop)
	{
		Schema.removeStatement(workFlowrdfDBModel, subject, object, prop);
	}

	public List<IWorkflowData> getUnresolvable()
	{
		return null;
	}

}
