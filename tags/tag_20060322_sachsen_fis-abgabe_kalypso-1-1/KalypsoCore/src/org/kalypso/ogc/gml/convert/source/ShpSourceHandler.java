package org.kalypso.ogc.gml.convert.source;

import java.net.URL;

import org.eclipse.core.resources.IFile;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.gml.util.ShpSourceType;
import org.kalypso.ogc.gml.convert.GmlConvertException;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.opengis.cs.CS_CoordinateSystem;

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
  public GMLWorkspace getWorkspace() throws GmlConvertException
  {
    try
    {
      final URL shpURL = m_resolver.resolveURL( m_context, m_href );
      
      final IFile file = ResourceUtilities.findFileFromURL( shpURL );
      final String absolutePath = file.getLocation().toFile().getAbsolutePath();
      final String shapeBase = FileUtilities.nameWithoutExtension( absolutePath );
      
      final ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
      final CS_CoordinateSystem sourceCrs = org.kalypsodeegree_impl.model.cs.Adapters.getDefault().export(
          csFac.getCSByName( m_crs ) );
      
      return ShapeSerializer.deserialize( shapeBase, sourceCrs );
    }
    catch( final Exception e )
    {
      throw new GmlConvertException( "Shape konnte nicht geladen werden: " + m_href, e );
    }
  }

}
