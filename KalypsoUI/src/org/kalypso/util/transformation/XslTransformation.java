package org.kalypso.util.transformation;

import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;

/**
 * @author belger
 */
public class XslTransformation implements CalculationCaseTransformation
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
    final String xsl = m_properties.getProperty( "xsl" );
    final String output = m_properties.getProperty( "output" );
    
    final IProject project = targetFolder.getProject();
    
    final IFile inputFile = (IFile)project.findMember( input );
    final IFile xslFile = (IFile)project.findMember( xsl );
    final IFile outputFile = targetFolder.getFile( output );
    
    System.out.println( "input exists:" + inputFile.exists() );
    System.out.println( "xsl exists:" + xslFile.exists() );

    try
    {
      // TODO: transform with xsl
      inputFile.copy( outputFile.getFullPath(), false, new NullProgressMonitor() );
    }
    catch( CoreException e )
    {
      throw new TransformationException( e );
    }
  }

}
