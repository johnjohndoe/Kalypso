package org.kalypso.loader.impl;

import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Properties;

import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree_impl.graphics.sld.SLDFactory;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;

/**
 * @author schlienger
 *  
 */
public class SLDLoader extends AbstractLoader
{
  public SLDLoader()
  {
  // nothing
  }

  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "OGC SLD";
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.util.Properties, org.eclipse.core.resources.IProject, org.eclipse.core.runtime.IProgressMonitor)
   */
  protected Object loadIntern( final Properties source, final IProject project, final IProgressMonitor monitor ) throws LoaderException
  {
    try
    {
      final String sourcePath = source.getProperty( "PATH", "" );
      final IFile file = project.getFile( sourcePath );

      final Reader reader = new InputStreamReader( file.getContents() );
      final StyledLayerDescriptor myStyledLayerDescriptor = SLDFactory.createSLD( reader );
      reader.close();

      return myStyledLayerDescriptor;
    }
    catch( Exception e )
    {
      throw new LoaderException( e );
    }
  }

  /**
   * 
   * @see org.kalypso.loader.ILoader#save(java.util.Properties,
   *      java.lang.Object)
   */
  public void save( final Properties source, final Object data ) throws LoaderException
  {
    // TODO: Transformation vom laden muss rueckgaengig gemacht werden !
    // TODO: support it
    throw new LoaderException( "Operation not supported" );
  }
}