package org.kalypso.model.transformation;

import java.util.Properties;

import org.eclipse.core.runtime.IProgressMonitor;

/**
 * @author belger
 */
public interface ICalculationCaseTransformation
{
  void setProperties( final Properties props );

  void transform( final IProgressMonitor monitor ) throws TransformationException;
}
