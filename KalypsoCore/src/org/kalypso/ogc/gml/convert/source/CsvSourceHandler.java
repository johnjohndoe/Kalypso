package org.kalypso.ogc.gml.convert.source;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.util.Iterator;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.core.i18n.Messages;
import org.kalypso.gml.util.CsvSourceType;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.ogc.gml.convert.GmlConvertException;
import org.kalypso.ogc.gml.serialize.CsvFeatureReader;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ogc.gml.serialize.CsvFeatureReader.CSVInfo;
import org.kalypsodeegree.model.feature.GMLWorkspace;

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
  public GMLWorkspace getWorkspace( ) throws GmlConvertException
  {
    final String href = m_type.getHref();
    final CsvFeatureReader reader = new CsvFeatureReader();
    final List propList = m_type.getFeatureproperty();
    InputStream stream = null;
    final ITypeRegistry<IMarshallingTypeHandler> typeRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    try
    {
      for( final Iterator propIt = propList.iterator(); propIt.hasNext(); )
      {
        final CsvSourceType.Featureproperty element = (CsvSourceType.Featureproperty) propIt.next();
        final List<Integer> columnList = element.getColumn();
        final int[] columns = new int[columnList.size()];
        for( int i = 0; i < columnList.size(); i++ )
        {
          final Integer col = columnList.get( i );
          columns[i] = col.intValue();
        }
        final QName qname = new QName( "namespace", element.getName() ); //$NON-NLS-1$
        final IMarshallingTypeHandler typeHandler = typeRegistry.getTypeHandlerForTypeName( element.getType() );
        final IPropertyType ftp = GMLSchemaFactory.createValuePropertyType( ShapeSerializer.PROPERTY_FEATURE_MEMBER, qname, typeHandler, 0, 1, false );
        final CSVInfo info = new CsvFeatureReader.CSVInfo( element.getFormat(), columns, element.isIgnoreFormatExceptions() );
        reader.addInfo( ftp, info );
      }
      final URL url = m_resolver.resolveURL( m_context, href );
      final URLConnection connection = url.openConnection();
      stream = connection.getInputStream();
      final String encoding = connection.getContentEncoding();
      final InputStreamReader isr = encoding == null ? new InputStreamReader( stream ) : new InputStreamReader( stream, encoding );
      return reader.loadCSV( isr, m_type.getComment(), m_type.getDelemiter(), m_type.getLineskip() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new GmlConvertException( Messages.getString("org.kalypso.ogc.gml.convert.source.CsvSourceHandler.1") + href, e ); //$NON-NLS-1$
    }
    finally
    {
      IOUtils.closeQuietly( stream );
    }
  }
}
