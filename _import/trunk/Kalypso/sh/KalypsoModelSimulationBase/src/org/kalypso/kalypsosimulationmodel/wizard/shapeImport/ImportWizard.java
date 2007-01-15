/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.wizard.shapeImport;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ShapeToIRoughnessCollection;

/**
 * @author antanas
 *
 */
public class ImportWizard extends Wizard implements INewWizard {
	// wizard pages
	PageMain pageMain;
	
	// the model
	WizardModel model;
	
	// workbench selection when the wizard was started
	protected IStructuredSelection selection;
	
	// flag indicated whether the wizard can be completed or not 
	protected boolean wizardCompleted = false;

	// the workbench instance
	protected IWorkbench workbench;

	public ImportWizard() {
		super();
		model = new WizardModel();
	}
	
	public void addPages()
	{
		pageMain = new PageMain(workbench, selection);
		addPage(pageMain);
	}

	/**
	 * @see IWorkbenchWizard#init(IWorkbench, IStructuredSelection)
	 */
	public void init(IWorkbench workbench, IStructuredSelection selection) 
	{
		this.workbench = workbench;
		this.selection = selection;
	}

	public boolean canFinish()
	{
		wizardCompleted = pageMain.canFlipToNextPage();
		return wizardCompleted;
	}
	
	public boolean performFinish() 
	{
		URL inputURL = null;
		URL outputURL = null;
		try {
			inputURL = new URL("file:"+model.inputFile);
			outputURL = new URL("file:"+model.outputFile);
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		final Job job = new ShapeToIRoughnessCollection( "Naslov", inputURL, outputURL, null, model.shapeProperty);
	    job.setUser( true );
	    job.schedule();

	    return false;
	}
}
