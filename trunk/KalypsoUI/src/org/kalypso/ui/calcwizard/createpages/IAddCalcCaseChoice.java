package org.kalypso.ui.calcwizard.createpages;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * @author belger
 */
public interface IAddCalcCaseChoice
{
  public void createControl( final Composite parent );
  
  public Control getControl();
  
  /** Erzeugt oder w�hlt einen Rechenfall und gibt dessen Basisverzeichnis zur�ck  */
  public IFolder perform( final IProgressMonitor monitor ) throws CoreException;
  
  /**
   * @see java.lang.Object#toString()
   */
  public String toString();
}