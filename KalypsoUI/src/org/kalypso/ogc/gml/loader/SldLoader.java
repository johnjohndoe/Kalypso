package org.kalypso.ogc.gml.loader;

import java.io.InputStreamReader;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;

import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree_impl.graphics.sld.SLDFactory;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
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

      final URL url = getSLDUrl( source, context );

      final Reader reader = new InputStreamReader( url.openStream() );
      final StyledLayerDescriptor styledLayerDescriptor = SLDFactory.createSLD( reader );
      reader.close();
      
      final IResource resource = ResourceUtilities.findFileFromURL( url );
      addResource( resource, styledLayerDescriptor );

      monitor.done();

      return styledLayerDescriptor;
    }
    catch( final Exception e )
    {
      throw new LoaderException( e );
    }
  }

  private URL getSLDUrl( final Properties source, final URL context ) throws MalformedURLException
  {
    final String sourcePath = source.getProperty( "PATH", "" );
    return new UrlResolver().resolveURL( context, sourcePath );
  }

  /**
   * @see org.kalypso.loader.ILoader#compareKeys(java.util.Properties, java.net.URL, java.util.Properties, java.net.URL)
   */
  public int compareKeys( final Properties source1, final URL context1, final Properties source2, final URL context2 )
  {
    try
    {
      final int url1Hash = getSLDUrl( source1, context1 ).hashCode();
      final int url2Hash = getSLDUrl( source2, context2 ).hashCode();
      
      return url1Hash - url2Hash;
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
    }
    
    return 0;
  }
}