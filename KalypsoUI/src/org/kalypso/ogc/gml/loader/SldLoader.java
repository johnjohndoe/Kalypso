package org.kalypso.ogc.gml.loader;

import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
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
public class SldLoader extends AbstractLoader
{
  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "OGC SLD";
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.util.Properties, java.net.URL, org.eclipse.core.runtime.IProgressMonitor)
   */
  protected Object loadIntern( Properties source, URL context, IProgressMonitor monitor ) throws LoaderException
  {
    // TODO: currently unsupported, remove deprecated one and implement this one
    throw new UnsupportedOperationException();
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
      
      addResource( file, project );

      return myStyledLayerDescriptor;
    }
    catch( final Exception e )
    {
      throw new LoaderException( e );
    }
  }
}