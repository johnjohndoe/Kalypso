package org.kalypso.ui.wizards.imports.roughness.junit_tests;

import junit.framework.TestCase;

import org.eclipse.jface.wizard.WizardDialog;
import org.kalypso.ui.wizards.imports.roughness.ImportWizard;

public class Test_ImportShapeWizard_Roughness extends TestCase {

	public final void testImportWizard() {
//		KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog(); //have to initialize catalog, to register feature types
		ImportWizard wizard = new ImportWizard();
			
		// Instantiates the wizard container with the wizard and opens it
		WizardDialog dialog = new WizardDialog( null, wizard);
		dialog.create();
		dialog.open();
	}

}
