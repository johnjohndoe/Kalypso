/**
 * 
 */
package org.kalypso.afgui.model;

import java.io.IOException;
import java.net.URL;

/**
 * @author Patrice Congo
 *
 */
public interface IWorkflowBuilder
{
	public IWorkflow createWorkflow(URL specURL, URL statusURL) throws IOException;
}
