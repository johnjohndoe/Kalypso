package org.kalypso.ogc.gml.loader;

import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.util.Properties;

import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree_impl.graphics.sld.SLDFactory;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.java.net.UrlUtilities;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.util.url.UrlResolver;

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
  protected Object loadIntern( final Properties source, final URL context, final IProgressMonitor monitor ) throws LoaderException
  {
    try
    {
      monitor.beginTask( "Lade SLD", 1000 );
      final String sourcePath = source.getProperty( "PATH", "" );
      
      final URL url = UrlResolver.resolveURL( context, sourcePath );
      

      final Reader reader = new InputStreamReader( url.openStream() );
      final StyledLayerDescriptor myStyledLayerDescriptor = SLDFactory.createSLD( reader );
      reader.close();
      
      final IResource resource = ResourceUtilities.findFileFromURL( url );
      addResource( resource, myStyledLayerDescriptor );

      monitor.done();
      
      return myStyledLayerDescriptor;
    }
    catch( final Exception e )
    {
      throw new LoaderException( e );
    }
  }
}