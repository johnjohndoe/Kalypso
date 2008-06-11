package org.kalypso.ogc.gml.convert.source;

import java.net.URL;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.xml.bind.JAXBElement;

import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.core.i18n.Messages;
import org.kalypso.gml.util.AddFeaturesMappingType;
import org.kalypso.gml.util.ChangeFeaturesMappingType;
import org.kalypso.gml.util.FeaturemappingSourceType;
import org.kalypso.gml.util.MappingType;
import org.kalypso.gml.util.SourceType;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.convert.GmlConvertException;
import org.kalypso.ogc.gml.convert.GmlConvertFactory;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.visitors.AddFeaturesToFeaturelist;
import org.kalypsodeegree_impl.model.feature.visitors.ChangeFeaturesFromFeaturelist;

/**
 * @author belger
 */
public class FeaturemappingSourceHandler implements ISourceHandler
{
  private final FeaturemappingSourceType m_source;

  private final URL m_context;

  private final IUrlResolver m_resolver;

  private final Map m_externData;

  public FeaturemappingSourceHandler( final IUrlResolver resolver, final URL context,
      final FeaturemappingSourceType source, final Map externData )
  {
    m_resolver = resolver;
    m_context = context;
    m_source = source;
    m_externData = externData;
  }

  /**
   * @throws GmlConvertException
   * @see org.kalypso.ogc.gml.convert.source.ISourceHandler#getWorkspace()
   */
  public GMLWorkspace getWorkspace() throws GmlConvertException
  {
    final List<JAXBElement<? extends SourceType>> sourceList = m_source.getSource();
    final Iterator<JAXBElement<? extends SourceType>> sourceIt = sourceList.iterator();

    // XSD schreibt vor, dass es genau 2 sources gibt
    final GMLWorkspace firstGML = GmlConvertFactory.loadSource( m_resolver, m_context, sourceIt.next().getValue(),
        m_externData );
    final GMLWorkspace secondGML = GmlConvertFactory.loadSource( m_resolver, m_context, sourceIt.next().getValue(),
        m_externData );

    final List<JAXBElement<? extends MappingType>> mappingList = m_source.getMapping();
    for( final JAXBElement< ? extends MappingType> name : mappingList )
    {
      final MappingType mapping = name.getValue();
      final String fromPath = mapping.getFromPath();
      final String toPath = mapping.getToPath();

      final FeatureList fromFeatures = getFeatureList( firstGML, fromPath );
      final String fromID = mapping.getFromID();
      final FeatureList toFeatures = getFeatureList( secondGML, toPath );
      final String toID = mapping.getToID();
      final IFeatureType toFeatureType = secondGML.getFeatureTypeFromPath( toPath );

      final Properties properties = new Properties();

      final List mapList = mapping.getMap();
      for( final Iterator mapIt = mapList.iterator(); mapIt.hasNext(); )
      {
        final MappingType.Map map = (MappingType.Map)mapIt.next();
        properties.setProperty( map.getFrom(), map.getTo() );
      }

      final FeatureVisitor visitor = createVisitor( mapping, toFeatures, toFeatureType, fromID, toID, properties );
      fromFeatures.accept( visitor );
    }

    return secondGML;
  }

  private FeatureVisitor createVisitor( final MappingType mapping, final FeatureList toFeatures,
      final IFeatureType toFeatureType, final String fromID, final String toID, final Properties properties )
      throws GmlConvertException
  {
    if( mapping instanceof AddFeaturesMappingType )
    {
      final AddFeaturesMappingType addType = (AddFeaturesMappingType)mapping;
      final String handleExisting = addType.getHandleExisting().value();
      final String fID = addType.getFid();
      return new AddFeaturesToFeaturelist( toFeatures, properties, toFeatureType, fromID, toID, handleExisting, fID );
    }
    else if( mapping instanceof ChangeFeaturesMappingType )
      return new ChangeFeaturesFromFeaturelist( toFeatures, properties, fromID, toID );
    else
      throw new GmlConvertException( Messages.getString("org.kalypso.ogc.gml.convert.source.FeaturemappingSourceHandler.0") + mapping.getClass().getName() ); //$NON-NLS-1$
  }

  private FeatureList getFeatureList( final GMLWorkspace workspace, final String path ) throws GmlConvertException
  {
    final Object featureFromPath = workspace.getFeatureFromPath( path );
    if( featureFromPath instanceof FeatureList )
      return (FeatureList)featureFromPath;

    throw new GmlConvertException( Messages.getString("org.kalypso.ogc.gml.convert.source.FeaturemappingSourceHandler.1") + path ); //$NON-NLS-1$
  }
}
