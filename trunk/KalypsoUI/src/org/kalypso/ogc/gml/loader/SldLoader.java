package org.kalypso.ogc.gml.loader;

import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;

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
  private final UrlResolver m_urlResolver;

  public SldLoader()
  {
    m_urlResolver = new UrlResolver();
  
  }

  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "OGC SLD";
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.lang.String, java.net.URL, org.eclipse.core.runtime.IProgressMonitor)
   */
  protected Object loadIntern( final String source, final URL context, final IProgressMonitor monitor ) throws LoaderException
  {
    try
    {
      monitor.beginTask( "Lade SLD", 1000 );

      final URL url = m_urlResolver.resolveURL( context, source );
      
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
}