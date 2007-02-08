/**
 * 
 */
package org.kalypso.afgui.model.impl;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.Platform;
import org.kalypso.afgui.model.IHelp;
import org.kalypso.afgui.model.IWorkflowPart;
import org.kalypso.afgui.schema.Schema;

import com.hp.hpl.jena.rdf.model.Resource;

/**
 * @author Patrice Congo
 *
 */
public class WorkflowPart implements IWorkflowPart
{
	final static private Logger logger= Logger.getLogger(WorkflowPart.class.getName());
    private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) );

    static
    {
      if( !log )
        logger.setUseParentHandlers( false );
    }
    
	protected Resource resource;
	
	
	public WorkflowPart(Resource resource)
	{
		this.resource=resource;
	}
	
	/**
	 * @see org.kalypso.afgui.model.IWorkflowPart#getHelp()
	 */
	public IHelp getHelp()
	{
		return  Schema.getHelp(resource);
	}

	/**
	 * @see org.kalypso.afgui.model.IActivity#getID()
	 */
	public String getID()
	{
		return resource.getURI();
	}

	/**
	 * @see org.kalypso.afgui.model.IWorkflowPart#getName()
	 */
	public String getName()
	{
		return Schema.getName(resource);
	}

	@Override
	public String toString()
	{
		return getName();
	}
	
	public Object getModelObject()
	{
		return resource;
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
		else if(obj instanceof IWorkflowPart)
		{
			try
			{
				return getURI().equals(((IWorkflowPart)obj).getURI());
			}
			catch(Throwable th)
			{
				logger.log(Level.SEVERE, "Exception while comparing:", th);
				return false;
			}
		}
		else
		{
			return false;
		}
	}

	public String getURI()
	{
		return resource.getURI();
	}
}
