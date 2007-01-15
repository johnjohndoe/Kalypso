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
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.opengis.cs.CS_CoordinateSystem;

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
			inputURL = new URL("file:"+model.inputFile); //$NON-NLS-1$
			outputURL = new URL("file:"+model.outputFile); //$NON-NLS-1$
		} catch (MalformedURLException e) {
			e.printStackTrace();
		}
		final CS_CoordinateSystem cs = ConvenienceCSFactory.getInstance().getOGCCSByName(model.coordinateSystem);
		final Job job = new ShapeToIRoughnessCollection( Messages.getString("ImportWizard.0"), inputURL, outputURL, cs, model.shapeProperty); //$NON-NLS-1$
	    job.setUser( true );
	    job.schedule();

	    //XXX use "return false" for JUnit testing; normally it is "return true"
	    return false;
	}
}
