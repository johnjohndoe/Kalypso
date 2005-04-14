package org.kalypso.ogc.gml.convert;

import java.net.URL;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import org.kalypso.gml.util.AddFeaturesMappingType;
import org.kalypso.gml.util.ChangeFeaturesMappingType;
import org.kalypso.gml.util.FeaturemappingSourceType;
import org.kalypso.gml.util.MappingType;
import org.kalypso.gml.util.SourceType;
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

  public FeaturemappingSourceHandler( final URL context, final FeaturemappingSourceType source )
  {
    m_context = context;
    m_source = source;
  }

  /**
   * @throws SourceHandlerException
   * @see org.kalypso.ogc.gml.convert.ISourceHandler#getWorkspace()
   */
  public GMLWorkspace getWorkspace() throws SourceHandlerException
  {
    final List sourceList = m_source.getSource();
    final Iterator sourceIt = sourceList.iterator();

    // XSD schreibt vor, dass es genau 2 sources gibt
    final GMLWorkspace firstGML = GmlConvertFactory.loadSource( m_context, (SourceType)sourceIt
        .next() );
    final GMLWorkspace secondGML = GmlConvertFactory.loadSource( m_context, (SourceType)sourceIt
        .next() );

    final List mappingList = m_source.getMapping();
    for( final Iterator mappingIt = mappingList.iterator(); mappingIt.hasNext(); )
    {
      final MappingType mapping = (MappingType)mappingIt.next();
      final String fromPath = mapping.getFromPath();
      final String toPath = mapping.getToPath();

      final FeatureList fromFeatures = getFeatureList( firstGML, fromPath );
      final FeatureList toFeatures = getFeatureList( secondGML, toPath );

      final Properties properties = new Properties();

      final List mapList = mapping.getMap();
      for( final Iterator mapIt = mapList.iterator(); mapIt.hasNext(); )
      {
        final MappingType.MapType map = (MappingType.MapType)mapIt.next();
        properties.setProperty( map.getFrom(), map.getTo() );
      }

      final FeatureVisitor visitor = createVisitor( mapping, toFeatures, properties );
      fromFeatures.accept( visitor );
    }

    return null;
  }

  private FeatureVisitor createVisitor( final MappingType mapping, final FeatureList toFeatures,
      final Properties properties ) throws SourceHandlerException
  {
    if( mapping instanceof AddFeaturesMappingType )
      return new AddFeaturesToFeaturelist( toFeatures, properties );
    else if( mapping instanceof ChangeFeaturesMappingType )
    {
      final ChangeFeaturesMappingType change = (ChangeFeaturesMappingType)mapping;

      final String sourceProperty = change.getSourceProperty();
      final String targetProperty = change.getTargetProperty();
      return new ChangeFeaturesFromFeaturelist( toFeatures, properties, sourceProperty,
          targetProperty );
    }
    else
      throw new SourceHandlerException( "Mapping-Type wird nicht unterstützt: " + mapping.getClass().getName() );
  }

  private FeatureList getFeatureList( final GMLWorkspace firstGML, final String fromPath )
      throws SourceHandlerException
  {
    final Object featureFromPath = firstGML.getFeatureFromPath( fromPath );
    if( featureFromPath instanceof FeatureList )
      return (FeatureList)featureFromPath;

    throw new SourceHandlerException( "Der FeaturePath der Quelle beschreibt keine FeatureListe: "
        + fromPath );
  }
}
