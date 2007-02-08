/**
 * 
 */
package org.kalypso.afgui.model.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;

import org.eclipse.core.runtime.Platform;
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
			Logger.getLogger(WorkflowData.class.getName());
    private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) );

    static
    {
      if( !log )
        logger.setUseParentHandlers( false );
    }
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
		logger.info("PropertyLinkToGet="+jenaProp+" Res="+resource.getModel());
		if(prop==null)
		{
			logger.warning("No Jena property for:"+prop);
			return false;
		}
		else 
		{	
			
			boolean hasProp=resource.getModel().contains(null, jenaProp, resource);//resource.hasProperty(jenaProp);
			logger.info("HasProp="+hasProp);
			return hasProp;
		}
	}
	
	public List<IWorkflowData> getLinkedWorkflowData(EWorkflowProperty prop)
	{
		Property jenaProp=Schema.toJenaProperty(prop);
		if(prop==null)
		{
			logger.warning("Cannot get jena property for:"+prop);
			return Collections.emptyList();
		}
		else
		{
//			StmtIterator stmIt=resource.listProperties(jenaProp);
//			List<IWorkflowData> list= new ArrayList<IWorkflowData>();
//			
//			Resource res;
//			
//			for(;stmIt.hasNext();)
//			{
//				res=(Resource)((Statement)stmIt.next()).getObject();
//				list.add(new WorkflowData(res));
//			}
			

			StmtIterator stmIt=
				resource.getModel().listStatements(null, jenaProp, resource);//
					//Properties(jenaProp);
			List<IWorkflowData> list= new ArrayList<IWorkflowData>();
			
			Resource res;
			
			for(;stmIt.hasNext();)
			{
				res=(Resource)((Statement)stmIt.next()).getSubject();
				list.add(new WorkflowData(res));
			}

			logger.info("ChildrenData:"+list);
			return list;
		}
	}

	/**
	 * @see org.kalypso.afgui.model.IWorkflowData#getName()
	 */
	public String getName()
	{
		return resource.getProperty(Schema.PROP_HAS_NAME).getString();//Object().toString();
	}
	
	@Override
	public String toString()
	{
		StringBuffer buffer= new StringBuffer("WorkflowData.");
		buffer.append(getName());
		return buffer.toString();
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(obj instanceof IWorkflowData)
		{
			try
			{
				return getURI().equals(((IWorkflowData)obj).getURI());
			}
			catch(Throwable th)
			{
				return false;
			}
		}
		else
		{
			return false;
		}

	}
	
	public void remove()
	{
		Schema.removeWorkflowData(resource);
	}

}
