package org.kalypso.ogc.gml.convert;

import java.net.URL;

import org.kalypso.gml.util.GmlSourceType;
import org.kalypso.java.net.IUrlResolver;
import org.kalypso.java.net.UrlUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author belger
 */
public class GmlSourceHandler implements ISourceHandler
{
  private String m_href;

  private IUrlResolver m_resolver = new UrlUtilities();

  private final URL m_context;

  public GmlSourceHandler( final URL context, final GmlSourceType type )
  {
    m_context = context;
    m_href = type.getHref();
  }

  /**
   * @throws SourceHandlerException
   * @see org.kalypso.ogc.gml.convert.ISourceHandler#getWorkspace()
   */
  public GMLWorkspace getWorkspace() throws SourceHandlerException
  {
    try
    {
      final URL gmlURL = m_resolver.resolveURL( m_context, m_href );
      return GmlSerializer.createGMLWorkspace( gmlURL, m_resolver );
    }
    catch( final Exception e )
    {
      throw new SourceHandlerException( "GML konnte nicht geladen werden: " + m_href, e );
    }
  }

}
