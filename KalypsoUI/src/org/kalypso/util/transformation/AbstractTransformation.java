package org.kalypso.util.transformation;

import java.io.BufferedWriter;
import java.util.Properties;

import org.eclipse.core.runtime.IProgressMonitor;

/**
 * @author belger
 */
public abstract class AbstractTransformation implements ITransformation
{
  private Properties m_properties;

  /**
   * @see org.kalypso.util.transformation.ITransformation#setProperties(java.util.Properties)
   */
  public final void setProperties( final Properties props )
  {
    m_properties = props;
  }

  /**
   * @see org.kalypso.util.transformation.ITransformation#transform(java.io.BufferedWriter,
   *      java.io.BufferedWriter, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void transform( final BufferedWriter msgWriter,
      final BufferedWriter logWriter, final IProgressMonitor monitor )
      throws TransformationException
  {
    transformIntern( m_properties, msgWriter, logWriter, monitor );
  }

  /**
   * @see ITransformation#transform(BufferedWriter, BufferedWriter,
   *      IProgressMonitor)
   * 
   * @param properties
   * @param msgWriter
   * @param logWriter
   * @param monitor
   * @throws TransformationException
   */
  protected abstract void transformIntern( final Properties properties,
      final BufferedWriter msgWriter, final BufferedWriter logWriter,
      final IProgressMonitor monitor ) throws TransformationException;
}