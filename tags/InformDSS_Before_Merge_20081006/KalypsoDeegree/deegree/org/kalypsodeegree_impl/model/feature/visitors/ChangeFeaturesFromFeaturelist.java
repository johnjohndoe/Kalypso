package org.kalypsodeegree_impl.model.feature.visitors;

import java.util.Map;
import java.util.Properties;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Ändert die Features einer {@link org.kalypsodeegree.model.feature.FeatureList}anhand der besuchten Features. Dazu
 * wird für jedes besuchte {@link org.kalypsodeegree.model.feature.Feature}ein passendes aus der Liste anhand einer
 * vorgegebenen zuordnung gesucht. Dann werden die Daten des besuchten Features via
 * {@link org.kalypsodeegree_impl.model.feature.FeatureHelper#copyProperties(Feature, Feature, Properties)}übertragen.
 * 
 * @author bce
 */
public class ChangeFeaturesFromFeaturelist implements FeatureVisitor
{
  private final Properties m_propertyMap;

  private final String m_sourceID;

  private Map m_index;

  public ChangeFeaturesFromFeaturelist( final FeatureList list, final Properties propertyMap, final String sourceID,
      final String targetID )
  {
    m_propertyMap = propertyMap;
    m_sourceID = sourceID;

    // index anhand der targetid erstellen
    final IndexFeaturesVisitor indexer = new IndexFeaturesVisitor( targetID );
    list.accept( indexer );
    m_index = indexer.getIndex();
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    final Object index = f.getProperty( m_sourceID);
    final Feature targetFeature = (Feature)m_index.get( index );
    if( targetFeature != null )
    {
      try
      {
        FeatureHelper.copyProperties( f, targetFeature, m_propertyMap );
      }
      catch( Exception e )
      {
        // TODO error handling
        e.printStackTrace();
      }
    }

    return true;
  }
}