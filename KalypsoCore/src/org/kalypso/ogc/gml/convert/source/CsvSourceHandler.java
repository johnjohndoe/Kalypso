package org.kalypso.ogc.gml.convert.source;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.kalypso.gml.util.CsvSourceType;
import org.kalypso.java.net.IUrlResolver;
import org.kalypso.ogc.gml.convert.GmlConvertException;
import org.kalypso.ogc.gml.serialize.CsvFeatureReader;
import org.kalypso.ogc.gml.serialize.CsvFeatureReader.CSVInfo;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author belger
 */
public class CsvSourceHandler implements ISourceHandler
{
  private final CsvSourceType m_type;

  private final URL m_context;

  private final IUrlResolver m_resolver;

  public CsvSourceHandler( final IUrlResolver resolver, final URL context, final CsvSourceType type )
  {
    m_resolver = resolver;
    m_context = context;
    m_type = type;
  }

  /**
   * @throws GmlConvertException
   * @see org.kalypso.ogc.gml.convert.source.ISourceHandler#getWorkspace()
   */
  public GMLWorkspace getWorkspace() throws GmlConvertException
  {
    final String href = m_type.getHref();
    
    final CsvFeatureReader reader = new CsvFeatureReader();
    
    final List propList = m_type.getFeatureproperty();
    for( final Iterator propIt = propList.iterator(); propIt.hasNext(); )
    {
      final CsvSourceType.FeaturepropertyType element = (CsvSourceType.FeaturepropertyType)propIt.next();
      
      final List columnList = element.getColumn();
      final int[] columns = new int[columnList.size()];
      for( int i = 0; i < columnList.size(); i++ )
      {
        final CsvSourceType.FeaturepropertyType.Column col = (CsvSourceType.FeaturepropertyType.Column)columnList.get( i );
        columns[i] = col.getValue();
      }
      
      final FeatureTypeProperty ftp = FeatureFactory.createFeatureTypeProperty( element.getName(), "namespace", element.getType(), false, null );
      final CSVInfo info = new CsvFeatureReader.CSVInfo( element.getFormat(), columns );
      reader.addInfo( ftp, info );
    }
    
    InputStream stream = null;
    try
    {
      final URL url = m_resolver.resolveURL( m_context, href );
      
      final URLConnection connection = url.openConnection();
      stream = connection.getInputStream();

      final String encoding = connection.getContentEncoding();
      final InputStreamReader isr = encoding == null ? new InputStreamReader( stream ) : new InputStreamReader( stream, encoding );
      
      return reader.loadCSV( isr, m_type.getComment(), m_type.getDelemiter(), m_type.getLineskip() );
    }
    catch( final Exception e )
    {
      throw new GmlConvertException( "CSV konnte nicht geladen werden: " + href, e );
    }
    finally
    {
      IOUtils.closeQuietly( stream );
    }
  }

}
