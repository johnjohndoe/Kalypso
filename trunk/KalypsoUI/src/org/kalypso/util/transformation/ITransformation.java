package org.kalypso.util.transformation;

import java.util.Properties;

import org.eclipse.core.runtime.IProgressMonitor;

/**
 * @author belger
 */
public interface ITransformation
{
  void setProperties( final Properties props );

  void transform( final IProgressMonitor monitor ) throws TransformationException;
}
