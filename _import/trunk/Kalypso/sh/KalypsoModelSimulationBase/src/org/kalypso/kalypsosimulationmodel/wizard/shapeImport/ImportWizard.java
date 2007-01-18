/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.wizard.shapeImport;

import java.io.File;

import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ShapeToIRoughnessCollection;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 *
 */
public class ImportWizard extends Wizard implements INewWizard {
	PageMain pageMain;		// wizard page		
	DataContainer m_data;	// the data model
	protected IStructuredSelection selection;	// workbench selection when the wizard was started
	protected boolean wizardCompleted = false;	// flag indicated whether the wizard can be completed or not
	//protected IWorkbench workbench;	// the workbench instance - NOT USED HERE

	public ImportWizard() {
		super();
		m_data = new DataContainer();
	}
	
	public void addPages()
	{
		pageMain = new PageMain(selection);
		addPage(pageMain);
	}

	/**
	 * @see IWorkbenchWizard#init(IWorkbench, IStructuredSelection)
	 */
	public void init(IWorkbench workbench, IStructuredSelection selection) 
	{
		//this.workbench = workbench;
		this.selection = selection;
	}

	public boolean canFinish()
	{
		wizardCompleted = pageMain.cmb_ShapeProperty.isEnabled() && pageMain.isTextNonEmpty(pageMain.txt_OutputFile);
		if(wizardCompleted)
		{
			m_data.inputFile = pageMain.txt_InputFile.getText();
			m_data.outputFile = pageMain.getResourceAbsolutePath() + File.separator + pageMain.txt_OutputFile.getText();
			m_data.shapeProperty = pageMain.cmb_ShapeProperty.getText();
			m_data.coordinateSystem = pageMain.cmb_CoordinateSystem.getText();
			m_data.description = pageMain.txt_Description.getText();
			m_data.project = pageMain.getTargetContainer().getProject();
		}
		return wizardCompleted;
	}
	
	public boolean performFinish() 
	{
		final Job job = new ShapeToIRoughnessCollection( Messages.getString("ImportWizard.0"), m_data); //$NON-NLS-1$
	    job.setUser( true );
	    job.schedule();

	    //XXX use "return false" for JUnit testing; normally "return true"
	    return true;
	}
}
