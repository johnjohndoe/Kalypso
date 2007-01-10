/**
 * 
 */
package org.kalypso.afgui.model.internal;

import org.apache.log4j.Logger;
import org.kalypso.afgui.db.Schema;
import org.kalypso.afgui.model.IHelp;
import org.kalypso.afgui.model.IWorkflowPart;

import com.hp.hpl.jena.rdf.model.Resource;

/**
 * @author Patrice Congo
 * 
 */
public class WorkflowPart implements IWorkflowPart {
	final static private Logger logger = Logger.getLogger(WorkflowPart.class);

	protected Resource resource;

	public WorkflowPart(Resource resource) {
		this.resource = resource;
	}

	/**
	 * @see org.kalypso.afgui.model.IWorkflowPart#getHelp()
	 */
	public IHelp getHelp() {
		return Schema.getHelp(resource);
	}

	/**
	 * @see org.kalypso.afgui.model.IActivity#getID()
	 */
	public String getID() {
		return resource.getURI();
	}

	/**
	 * @see org.kalypso.afgui.model.IWorkflowPart#getName()
	 */
	public String getName() {
		return Schema.getName(resource);
	}

	@Override
	public String toString() {
		return getName();
	}

	public Object getModelObject() {
		return resource;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null) {
			return false;
		} else if (this == obj) {
			return true;
		} else if (obj instanceof IWorkflowPart) {
			try {
				return getURI().equals(((IWorkflowPart) obj).getURI());
			} catch (Throwable th) {
				logger.error("Exception while comparing:", th);
				return false;
			}
		} else {
			return false;
		}
	}

	public String getURI() {
		return resource.getURI();
	}
}
