package org.kalypso.ui.wizards.imports._tests;

import junit.framework.TestCase;

import org.eclipse.jface.wizard.WizardDialog;
import org.kalypso.ui.wizards.imports.roughness.ImportWizard;

import test.org.kalypso.kalypsosimulationmodel.TestUtils;

public class Test_WizardImportRoughness extends TestCase
{

  public final void testImportWizard( )
  {
    try
    {
      ImportWizard wizard = new ImportWizard();
      WizardDialog dialog = new WizardDialog( null, wizard );
      dialog.create();
      dialog.open();
    }
    catch(Throwable th)
    {
      fail(TestUtils.getStackTraceAsString( th ));
    }
  }

}
