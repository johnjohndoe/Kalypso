package org.kalypso.afgui.db;

import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.afgui.schema.Schema;

import com.hp.hpl.jena.rdf.model.Model;

public class WorkflowDB implements IWorkflowDB
{
	
	final private Map< String, IWorkflowDataCreationMechanism> 
				creationMechanism= new Hashtable<String, IWorkflowDataCreationMechanism>();
	
	private Model dbModel;
	
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
		
		return creator.create(dbModel, id, type, parent);
	}

	public IWorkflowData derivedWorkflowData(
									IWorkflowData parent, 
									String childId)
	{
		return Schema.derivedWorkflowData(dbModel, parent, childId);
	}

	public IWorkflowData getWorkflowDataById(String id)
	{
		return Schema.getWorkflowDataById(dbModel, id);
	}

	public List<IWorkflowData> getWorkflowDataByType(String type)
	{
		return Schema.getWorkflowDataByType(dbModel, type);
	}
	
	

	public void link(
				IWorkflowData subject, 
				IWorkflowData object, 
				EWorkflowProperty prop)
	{
		Schema.createStatement(dbModel, subject, object, prop);
	}

	public void unlink(
					IWorkflowData subject, 
					IWorkflowData object, 
					EWorkflowProperty prop)
	{
		Schema.removeStatement(dbModel, subject, object, prop);
	}

	public List<IWorkflowData> getUnresolvable()
	{
		return null;
	}
	
	public List<IWorkflowData> getRootWorkflowDataByType(String type)
	{
		return Schema.getRootWorkflowDataByType(dbModel, type);
	}
}
