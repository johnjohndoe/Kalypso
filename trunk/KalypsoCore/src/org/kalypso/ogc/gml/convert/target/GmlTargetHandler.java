package org.kalypso.ogc.gml.convert.target;

import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.kalypso.gml.util.Gmltarget;
import org.kalypso.java.net.IUrlResolver;
import org.kalypso.ogc.gml.convert.GmlConvertException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author belger
 */
public class GmlTargetHandler implements ITargetHandler
{
  private final Gmltarget m_target;

  private final IUrlResolver m_resolver;

  private final URL m_context;

  public GmlTargetHandler( final IUrlResolver resolver, final URL context, final Gmltarget target )
  {
    m_resolver = resolver;
    m_context = context;
    m_target = target;
  }

  /**
   * @see org.kalypso.ogc.gml.convert.target.ITargetHandler#saveWorkspace(org.kalypsodeegree.model.feature.GMLWorkspace)
   */
  public void saveWorkspace( final GMLWorkspace workspace ) throws GmlConvertException
  {
    BufferedWriter writer = null;
    try
    {
      final String href = m_target.getHref();

      final URL url = m_resolver.resolveURL( m_context, href );

      final OutputStreamWriter osw = m_resolver.createWriter( url );
      writer = new BufferedWriter( osw );

      GmlSerializer.serializeWorkspace( writer, workspace, osw.getEncoding() );
    }
    catch( final Exception e )
    {
      throw new GmlConvertException( "Daten wurden nicht gespeichert", e );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

}
