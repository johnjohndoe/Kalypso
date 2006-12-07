package org.kalypso.kalypso1d2d.pjt.util;

import org.apache.log4j.Logger;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.wizards.datatransfer.FileSystemImportWizard;
import org.kalypso.afgui.db.IWorkflowDB;
import org.kalypso.kalypso1d2d.pjt.ActiveWorkContext;
import org.kalypso.kalypso1d2d.pjt.wizards.NewSimulationModelWizardPage;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;

public class ImportUtils
{
	private static final Logger logger= Logger.getLogger(ImportUtils.class);
	
	public void importGML(Shell parentShell)
	{
		FileSystemImportWizard fsiWizard= new FileSystemImportWizard();
		
		WizardDialog wd= new WizardDialog(parentShell,fsiWizard);
		wd.setTitle("Import GML");
		int decision=wd.open();
		if(decision==WizardDialog.OK)
		{
			
		}
		else
		{
			logger.info("Wizard canceled:"+decision);
		}
	}
}
