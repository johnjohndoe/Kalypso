package org.kalypso.ui.shapeImportWizards.utils.importRoughness.junit_tests;

import junit.framework.TestCase;

import org.eclipse.jface.wizard.WizardDialog;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.ui.shapeImportWizards.utils.importRoughness.ImportWizard;

public class Test_ImportShapeWizard_Roughness extends TestCase {

	protected void setUp() throws Exception {
		super.setUp();
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	public final void testImportWizard() {
//		KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog(); //have to initialize catalog, to register feature types
		ImportWizard wizard = new ImportWizard();
			
		// Instantiates the wizard container with the wizard and opens it
		WizardDialog dialog = new WizardDialog( null, wizard);
		dialog.create();
		dialog.open();
	}

}
