/**
 * 
 */
package org.kalypso.afgui.model.impl;

import org.kalypso.afgui.db.EWorkflowProperty;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.afgui.schema.Schema;

import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.vocabulary.RDF;

/**
 * @author Patrice Congo
 *
 */
public class WorkflowData implements IWorkflowData
{
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

}
