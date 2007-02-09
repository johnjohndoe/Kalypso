package org.kalypso.ui.wizards.imports.tests;

import junit.framework.TestCase;

import org.eclipse.jface.wizard.WizardDialog;
import org.kalypso.ui.wizards.imports.roughness.ImportWizard;

public class Test_WizardImportRoughness extends TestCase
{

  public final void testImportWizard( )
  {
    ImportWizard wizard = new ImportWizard();
    WizardDialog dialog = new WizardDialog( null, wizard );
    dialog.create();
    dialog.open();
  }

}
