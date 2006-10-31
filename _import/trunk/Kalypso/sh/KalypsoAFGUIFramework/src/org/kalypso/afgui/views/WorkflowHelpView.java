/**
 * 
 */
package org.kalypso.afgui.views;

import org.apache.log4j.Logger;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.model.EActivityAction;
import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.events.WorkflowChangeEvent;
import org.kalypso.afgui.model.events.WorkflowChangeEventListerner;

import com.hp.hpl.jena.reasoner.rdfsReasoner1.BRWRule;

/**
 * @author pat_dev
 *
 */
public class WorkflowHelpView extends ViewPart
{
	private Browser browser;
	final static private String BLANK_HTML="<html></html>";
	final static private Logger logger=
				Logger.getLogger(WorkflowHelpView.class);
	final static public String ID="org.kalypso.afgui.views.WorkflowHelpView";
	private WorkflowChangeEventListerner wfceListener=
		new WorkflowChangeEventListerner()
		{

			public void onWorkflowChanged(WorkflowChangeEvent event)
			{
				logger.info("*+*+*+*+*+*+*+*\n"+event);
				IWorkflow workflow=(IWorkflow)event.getSource();
				if(!(workflow.getRuntineStatus().getCurrentAction()==
												EActivityAction.GET_HELP))
				{
					browser.setText(BLANK_HTML);
				}
				else
				{
					IActivity activity=
						 workflow.getCurrentActivity();
					String html;
					if(activity==null)
					{
						html=BLANK_HTML;
					}
					else
					{
						html=activity.getHelp();
						if(html==null)
						{
							html=BLANK_HTML;
						}
					}
					browser.setText(html);
				}
				
			}
			
		};
		
	/* (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createPartControl(Composite parent)
	{
		browser= new Browser(parent,SWT.FILL);
		browser.setText(BLANK_HTML);
		IWorkflow workflow=
			KalypsoAFGUIFrameworkPlugin.getDefault().getWorkflow();
		workflow.addWorkflowChangedEventListener(wfceListener);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
	 */
	@Override
	public void setFocus()
	{
		browser.setFocus();

	}

}
