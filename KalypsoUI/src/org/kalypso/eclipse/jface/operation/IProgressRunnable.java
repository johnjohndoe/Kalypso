package org.kalypso.eclipse.jface.operation;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;

/**
 * @author belger
 */
public interface IProgressRunnable
{
  public IStatus run( final IProgressMonitor monitor );
}
