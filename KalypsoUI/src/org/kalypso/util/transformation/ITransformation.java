package org.kalypso.util.transformation;

import java.io.BufferedWriter;
import java.util.Properties;

import org.eclipse.core.runtime.IProgressMonitor;

/**
 * @author belger
 */
public interface ITransformation
{
  void setProperties( final Properties props );

  /**
   * Performs the transformation.
   * 
   * @param msgWriter write short descriptions or summary of the real problem in this writer. The contents are held in memory.
   * @param logWriter write full descriptions of the problem in this writer. The contents are written to a file.
   * @param monitor
   * @throws TransformationException
   */
  void transform( final BufferedWriter msgWriter, final BufferedWriter logWriter, final IProgressMonitor monitor ) throws TransformationException;
}
