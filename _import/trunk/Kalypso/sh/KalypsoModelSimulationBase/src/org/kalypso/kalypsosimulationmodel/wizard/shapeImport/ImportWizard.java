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
		return wizardCompleted;
	}
	
	public boolean performFinish() 
	{
		m_data.setInputFile(pageMain.txt_InputFile.getText());
		m_data.setOutputFile(pageMain.getResourceAbsolutePath() + File.separator + pageMain.txt_OutputFile.getText());
		m_data.setShapeProperty(pageMain.cmb_ShapeProperty.getText());
		m_data.setCoordinateSystem(pageMain.cmb_CoordinateSystem.getText());
		m_data.setDescription(pageMain.txt_Description.getText());
		m_data.setProject(pageMain.getTargetContainer().getProject());
		m_data.setCreateMap(pageMain.btn_CheckBox_CreateGMT.getSelection());
		m_data.setOutputDirectory(pageMain.getResourceRelativePath());
		final Job job = new ShapeToIRoughnessCollection( Messages.getString("ImportWizard.0"), m_data); //$NON-NLS-1$
	    job.setUser( true );
	    job.schedule();

	    //XXX use "return false" for JUnit testing; normally "return true"
	    return true;
	}
}
