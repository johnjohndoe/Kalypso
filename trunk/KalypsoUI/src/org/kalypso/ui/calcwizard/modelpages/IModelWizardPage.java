package org.kalypso.ui.calcwizard.modelpages;

import java.util.Properties;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.IWizardPage;

/**
 * @author belger
 */
public interface IModelWizardPage extends IWizardPage
{
  public void init( final IProject project, final String pagetitle,
      final ImageDescriptor imagedesc, final Properties arguments, final IFolder calcFolder );
  
  public boolean performFinish();
}
