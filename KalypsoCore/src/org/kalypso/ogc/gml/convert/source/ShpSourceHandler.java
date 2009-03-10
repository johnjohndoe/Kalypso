package org.kalypso.ogc.gml.convert.source;

import java.net.URL;

import org.eclipse.core.resources.IFile;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.core.i18n.Messages;
import org.kalypso.gml.util.ShpSourceType;
import org.kalypso.ogc.gml.convert.GmlConvertException;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author belger
 */
public class ShpSourceHandler implements ISourceHandler
{
  private String m_href;

  private final URL m_context;

  private final IUrlResolver m_resolver;

  private final String m_crs;

  public ShpSourceHandler( final IUrlResolver resolver, final URL context, final ShpSourceType type )
  {
    m_resolver = resolver;
    m_context = context;
    m_href = type.getHref();
    m_crs = type.getCrs();
  }

  /**
   * @throws GmlConvertException
   * @see org.kalypso.ogc.gml.convert.source.ISourceHandler#getWorkspace()
   */
  public GMLWorkspace getWorkspace( ) throws GmlConvertException
  {
    try
    {
      final URL shpURL = m_resolver.resolveURL( m_context, m_href );

      final IFile file = ResourceUtilities.findFileFromURL( shpURL );
      final String absolutePath = file.getLocation().toFile().getAbsolutePath();
      final String shapeBase = FileUtilities.nameWithoutExtension( absolutePath );

      return ShapeSerializer.deserialize( shapeBase, m_crs );
    }
    catch( final Exception e )
    {
      throw new GmlConvertException( Messages.getString("org.kalypso.ogc.gml.convert.source.ShpSourceHandler.0") + m_href, e ); //$NON-NLS-1$
    }
  }

}
