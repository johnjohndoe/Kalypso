package org.kalypso.ui.calcwizard;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.wizard.IWizardPage;

/**
 * @author belger
 */
public interface ICalcWizardPage extends IWizardPage
{
  public void doNext( final IProgressMonitor monitor ) throws CoreException;
  
  public void clean( final IProgressMonitor monitor ) throws CoreException;
}
