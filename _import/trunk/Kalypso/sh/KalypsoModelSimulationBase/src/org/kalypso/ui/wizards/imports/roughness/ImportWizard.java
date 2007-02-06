/**
 * 
 */
package org.kalypso.ui.wizards.imports.roughness;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 * 
 */
public class ImportWizard extends Wizard implements INewWizard
{
	protected DataContainer m_data; // the data model

	private PageMain pageMain;

	protected PageOutputFeature pageOutputFeature;

	protected PageOutputFile pageOutputFile;

	// private PageStyleSelection pageStyleSelection;

	// workbench selection when the wizard was started
	protected IStructuredSelection selection;

	// flag indicated whether the wizard can be completed or not
	protected boolean wizardCompleted = false;

	protected IWorkbench workbench; // the workbench instance - NOT USED HERE

	public ImportWizard()
	{
		super();
		setWindowTitle("Shape import");
		m_data = new DataContainer();
		setNeedsProgressMonitor(true);
	}

	/**
	 * @see IWorkbenchWizard#init(IWorkbench, IStructuredSelection)
	 */
	public void init(IWorkbench iWorkbench, IStructuredSelection iSelection)
	{
		this.workbench = iWorkbench;
		this.selection = iSelection;
	}

	@Override
	public void addPages()
	{
		if (this.selection == null) this.selection = new StructuredSelection();
		pageMain = new PageMain(m_data);
		pageOutputFile = new PageOutputFile(selection);
		pageOutputFeature = new PageOutputFeature();
		// pageGmlFileImport.setProjectSelection( m_outlineviewer.getMapModell().getProject() );
		// pageStyleSelection = new PageStyleSelection();

		addPage(pageMain);
		addPage(pageOutputFeature);
		addPage(pageOutputFile);
		// addPage(pageStyleSelection);
	}

	@Override
	public boolean canFinish()
	{
		WizardPage pageSecond = m_data.isOutputToFile() ? pageOutputFile : pageOutputFeature;
		wizardCompleted = pageMain.canFlipToNextPage() && pageSecond.isPageComplete();
		return wizardCompleted;
	}

	@Override
	public boolean performFinish()
	{
		pageMain.saveDataToModel();
        pageOutputFile.saveDataToModel();
		m_data.setSelectedFeature(pageOutputFeature.getSelectedFeature());
		// Feature[] features = pageFeatureSelection.getFeatures();
		final ICoreRunnableWithProgress operation = new TransformerShapeToIRoughnessCollection(m_data); //$NON-NLS-1$
		final IStatus status = RunnableContextHelper.execute(getContainer(), true, true, operation);
		ErrorDialog.openError(getShell(), getWindowTitle(), "", status);
		return status.isOK();
	}
}
