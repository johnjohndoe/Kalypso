package org.kalypso.util.transformation;

import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * @author belger
 */
public class CopyTransformation implements CalculationCaseTransformation
{
  private Properties m_properties;

  /**
   * @see org.kalypso.util.transformation.CalculationCaseTransformation#setProperties(java.util.Properties)
   */
  public void setProperties( final Properties props )
  {
    m_properties = props;
  }

  /**
   * @throws TransformationException
   * @see org.kalypso.util.transformation.CalculationCaseTransformation#transform(org.eclipse.core.resources.IFolder, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void transform( final IFolder targetFolder, final IProgressMonitor monitor ) throws TransformationException
  {
    final String input = m_properties.getProperty( "input" );
    final String output = m_properties.getProperty( "output" );

    if( input == null )
      throw new TransformationException( "Parameter 'input' nicht gesetzt" );
    if( output == null || output.length() == 0 )
      throw new TransformationException( "Parameter 'output' nicht gesetzt" );
    
    final IProject project = targetFolder.getProject();
    
    final IResource inputResource = project.findMember( input );
    if( inputResource == null || !inputResource.exists() || !( inputResource instanceof IFile ) )
      throw new TransformationException( "input file doesn't exist or is not a file: " + input );

    final IFile inputFile = (IFile)inputResource;
    final IFile outputFile = targetFolder.getFile( output );

    try
    {
      inputFile.copy( outputFile.getFullPath(), false, monitor );
    }
    catch( final CoreException e )
    {
      throw new TransformationException( e );
    }
  }

}
