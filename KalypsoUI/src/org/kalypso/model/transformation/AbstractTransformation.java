package org.kalypso.model.transformation;

import java.util.Properties;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * @author belger
 */
public abstract class AbstractTransformation implements ICalculationCaseTransformation
{
  private Properties m_properties;

  /**
   * @see org.kalypso.model.transformation.ICalculationCaseTransformation#setProperties(java.util.Properties)
   */
  public final void setProperties( final Properties props )
  {
    m_properties = props;
  }

  /**
   * @throws TransformationException
   * @see org.kalypso.model.transformation.ICalculationCaseTransformation#transform(org.eclipse.core.resources.IFolder, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void transform( final IFolder targetFolder, final IProgressMonitor monitor ) throws TransformationException
  {
    transformIntern( targetFolder, m_properties, monitor );
  }

  protected abstract void transformIntern( final IFolder targetFolder, final Properties properties, final IProgressMonitor monitor ) throws TransformationException;
}
