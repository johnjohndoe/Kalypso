package org.kalypso.util.transformation;

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
   * @see org.kalypso.util.transformation.ITransformation#transform(org.eclipse.core.runtime.IProgressMonitor)
   */
  public void transform( final IProgressMonitor monitor ) throws TransformationException
  {
    transformIntern( m_properties, monitor );
  }

  protected abstract void transformIntern( final Properties properties, final IProgressMonitor monitor ) throws TransformationException;
}
