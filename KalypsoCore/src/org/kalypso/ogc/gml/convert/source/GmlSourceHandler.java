package org.kalypso.ogc.gml.convert.source;

import java.net.URL;

import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.core.i18n.Messages;
import org.kalypso.gml.util.GmlSourceType;
import org.kalypso.ogc.gml.convert.GmlConvertException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author belger
 */
public class GmlSourceHandler implements ISourceHandler
{
  private String m_href;

  private final URL m_context;

  private final IUrlResolver m_resolver;

  public GmlSourceHandler( final IUrlResolver resolver, final URL context, final GmlSourceType type )
  {
    m_resolver = resolver;
    m_context = context;
    m_href = type.getHref();
  }

  /**
   * @throws GmlConvertException
   * @see org.kalypso.ogc.gml.convert.source.ISourceHandler#getWorkspace()
   */
  public GMLWorkspace getWorkspace() throws GmlConvertException
  {
    try
    {
      final URL gmlURL = m_resolver.resolveURL( m_context, m_href );
      return GmlSerializer.createGMLWorkspace( gmlURL, null );
    }
    catch( final Exception e )
    {
      throw new GmlConvertException( Messages.getString("org.kalypso.ogc.gml.convert.source.GmlSourceHandler.0") + m_href, e ); //$NON-NLS-1$
    }
  }

}
