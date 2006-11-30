/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.wizards;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.views.framelist.UpAction;
import org.kalypso.afgui.model.IWorkflowData;

/**
 * @author pat_dev
 *
 */
public class NewSimulationModelWizardPage extends WizardPage
{
	private IWorkflowData workflowData;
	
	private NewSimulationModelControlBuilder c;
	
	IUpdateListener updateListener= new IUpdateListener()
	{
		public void update()
		{
			boolean status=c.isValid();
			if(status)
			{
				NewSimulationModelWizardPage.this.setErrorMessage(null);
			}
			else
			{
				NewSimulationModelWizardPage.this.setErrorMessage(c.getErrorMessage());
			}
			NewSimulationModelWizardPage.this.setPageComplete(status);
		}
	};
	
	public NewSimulationModelWizardPage(
								String pageName,
								IWorkflowData workflowData)
	{
		super(pageName);
		super.setTitle(pageName);
		this.workflowData=workflowData;
	}

	
	
	public void createControl(Composite parent)
	{
		c= new NewSimulationModelControlBuilder(null,parent);
		setControl(c.getControl());
		c.setUpdateListerner(updateListener);
		updateListener.update();
	}
	
	
	@Override
	public boolean isPageComplete()
	{
		return super.isPageComplete();
	}			
	
	public NewSimulationModelControlBuilder getNewSimulaionControlBuilder()
	{
		return c;
	}
}