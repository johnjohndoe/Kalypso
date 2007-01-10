package org.kalypso.afgui.model.internal;

import org.kalypso.afgui.model.IActivity;

import com.hp.hpl.jena.rdf.model.Resource;

/**
 * Provide the default implementation of {@link IActivity}
 * 
 * @author Patrice Congo
 */
public class Activity extends WorkflowPart implements IActivity {

	/**
	 * Create an Activity base on the provided specification and having the
	 * given status
	 * 
	 * @param spec --
	 *            the spec of the activity to create
	 * @param status --
	 *            the status of the activity to create
	 */
	public Activity(Resource resource) {
		super(resource);
	}

	@Override
	public String toString() {
		StringBuffer buffer = new StringBuffer("Activity.");
		buffer.append(getName());
		return buffer.toString();
	}

}
