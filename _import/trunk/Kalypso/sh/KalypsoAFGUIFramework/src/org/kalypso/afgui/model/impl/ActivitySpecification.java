/**
 * 
 */
package org.kalypso.afgui.model.impl;

import java.util.List;
import java.util.logging.Logger;

import org.eclipse.core.runtime.Platform;
import org.kalypso.afgui.model.IActivityHelp;
import org.kalypso.afgui.model.IActivitySpecification;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.afgui.schema.Schema;

import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;

/**
 * @author Patrice Congo
 *
 */
public class ActivitySpecification implements
		IActivitySpecification
{
	/**
	 * Logger for the Activity specification class
	 */
	final static Logger logger=
		Logger.getLogger(ActivitySpecification.class.getName());
    private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) );

    static
    {
      if( !log )
        logger.setUseParentHandlers( false );
    }
    
	/** the resource representing this activity*/
	final private Resource activity; 
	
	/**
	 * Create an ActivitySpecification object based on the provided 
	 * activity.
	 * @param activity the resource representing the activity
	 */
	public ActivitySpecification(Resource activity)
	{
		if(activity==null)
		{
			throw new IllegalArgumentException(
					"Param activity must not be null");
		}
		this.activity=activity;
	}

	/**
	 * @see org.kalypso.afgui.model.IActivitySpecification#getAssertions()
	 */
	public List<Object> getAssertions()
	{
		throw new RuntimeException("Not implemented yet");
		//return null;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IActivitySpecification#getDataModel()
	 */
	public IWorkflowData getDataModel()
	{
		throw new RuntimeException("not implemented yet");
		//return null;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IActivitySpecification#getDescription()
	 */
	public String getDescription()
	{
		return activity.getProperty(Schema.PROP_HAS_NAME).getObject().toString();
	}

	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IActivitySpecification#getHelp()
	 */
	public IActivityHelp getHelp()
	{
//		final String help=getHelpString(activity);
		return new IActivityHelp()
		{

			public String getHelp()
			{
				return "<html><body> Help for"+getName()+
						"</body></html>";
			}

			public int getType()
			{
				return IActivityHelp.HELP_TYPE_HTML;
			}
			
		};
	}
	
//	private static final String getHelpString(Resource activity)
//	{
//		Statement stm=
//			activity.getProperty(Schema.PROP_HAS_HELP);
//		if(stm==null)
//		{
//			return null;
//		}
//		else 
//		{
//			RDFNode object=stm.getObject();
//			if(object.isLiteral())
//			{
//				return object.toString();
//			}
//			else
//			{
//				logger.warning(
//						"Help is an object change implementation to cope:"+object);
//				return object.toString();
//			}
//		}
//			
//	}
	
	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IActivitySpecification#getName()
	 */
	public String getName()
	{
		return activity.getProperty(
							Schema.PROP_HAS_NAME).getObject().toString();
	}
	
	public String getID()
	{
		return activity.getURI();
	}

	/**
	 * @see org.kalypso.afgui.model.IActivitySpecification#getWorkPannel()
	 */
	public Class getWorkPannel()
	{
		return null;
	}

	@Override
	public String toString()
	{
		StringBuffer buffer= new StringBuffer();
		buffer.append("ASpec[");
		buffer.append(getName());
		buffer.append("]");
		
		return buffer.toString();
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
		else if(obj instanceof IActivitySpecification)
		{
			return getID().equals(((IActivitySpecification)obj).getID());
		}
		else
		{
			return false;
		}
	}

	public boolean isRoot()
	{
		Statement stm=activity.getProperty(Schema.PROP_IS_ROOT);
		logger.info("\nisRoot:"+stm);
		logger.info(activity.toString());
		if(stm==null)
		{
			return false;
		}
		else
		{
			try
			{
				return stm.getBoolean();
			}
			catch(Throwable th)
			{
				return false;
			}
		}
	}
}
