/**
 * 
 */
package org.kalypso.afgui.model.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.log4j.Logger;
import org.kalypso.afgui.db.EWorkflowProperty;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.afgui.schema.Schema;

import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.vocabulary.RDF;

/**
 * @author Patrice Congo
 *
 */
public class WorkflowData implements IWorkflowData
{
	final static private Logger logger=
			Logger.getLogger(WorkflowData.class);
	private final Resource resource;
	
	
	public WorkflowData(Resource resource)
	{
		if(resource==null)
		{
			throw new IllegalArgumentException(
					"Param resource must not be null");
		}
		if(!resource.hasProperty(RDF.type, Schema.CLASS_WORKFLOW_DATA))
		{
			throw new IllegalArgumentException(
						"Associated type is not a workflow data:"+resource);
		}
		this.resource=resource;
		
	}

	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IWorkflowData#getType()
	 */
	public String getType()
	{
		return Schema.getStringProperty(resource, EWorkflowProperty.HAS_TYPE);

	}

	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IWorkflowData#getResourceURI()
	 */
	public String getLocation()
	{
		return Schema.getStringProperty(resource, EWorkflowProperty.HAS_LOCATION);
		
	}
	
	public String getURI()
	{
		return resource.getURI();
	}

	public Object getModelObject()
	{
		return resource;
	}
	
	public boolean hasLinkedWorkflowData(EWorkflowProperty prop)
	{
		Property jenaProp=Schema.toJenaProperty(prop);
		if(prop==null)
		{
			return false;
		}
		else 
		{	
			return resource.hasProperty(jenaProp);
		}
	}
	
	public List<IWorkflowData> getLinkedWorkflowData(EWorkflowProperty prop)
	{
		Property jenaProp=Schema.toJenaProperty(prop);
		if(prop==null)
		{
			logger.warn("Cannot get jena property for:"+prop);
			return Collections.emptyList();
		}
		else
		{
			StmtIterator stmIt=resource.listProperties(jenaProp);
			List<IWorkflowData> list= new ArrayList<IWorkflowData>();
			
			Resource res;
			
			for(;stmIt.hasNext();)
			{
				res=(Resource)((Statement)stmIt.next()).getObject();
				list.add(new WorkflowData(res));
			}
			return list;
		}
	}

}
