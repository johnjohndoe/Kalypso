package org.kalypso.util.transformation;

import java.util.Properties;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * @author belger
 */
public interface CalculationCaseTransformation
{
  void setProperties( final Properties props );

  void transform( final IFolder targetFolder, final IProgressMonitor monitor ) throws TransformationException;
}
