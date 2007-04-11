package org.kalypso.kalypsomodel1d2d.ui.map.flowrel.wizardPageZmlImportWithPreview;

import org.eclipse.jface.wizard.Wizard;

public class WizardZmlChooser extends Wizard
{

	@Override
	public void addPages()
	{
		// create and add first page
		WizardPageZmlChooser cPage = new WizardPageZmlChooser("CHtml");
		cPage.setTitle("Html File");
		cPage.setDescription("Enter file name.");
		addPage(cPage);
	}

	@Override
	public boolean performFinish()
	{
		// TODO Auto-generated method stub
		return false;
	}

}
