package org.kalypsodeegree_impl.model.feature.visitors;

import java.util.Map;
import java.util.Properties;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Fügt neue Features in eine {@link org.kalypsodeegree.model.feature.FeatureList} ein.
 * Dabei wird für jedes besuchte Feature ein neues (Default) Feature erzeugt in welches
 * Werte aus dem besuchten Feature via {@link org.kalypsodeegree_impl.model.feature.FeatureHelper#copyProperties(Feature, Feature, Properties)}
 * übertragen werden. 
 * 
 * @author bce
 */
public class AddFeaturesToFeaturelist implements FeatureVisitor
{
  private final FeatureList m_list;
  private final Properties m_propertyMap;
  private final FeatureType m_featureType;
  private final String m_fromID;
  private final Map m_idHash;
  private final boolean m_overwriteExisting;
  
  private int m_id = 0;

  public AddFeaturesToFeaturelist( final FeatureList list, final Properties propertyMap, final FeatureType featureType, final String fromID, final String toID, final boolean overwriteExisting )
  {
    m_list = list;
    m_featureType = featureType;
    m_propertyMap = propertyMap;
    m_fromID = fromID;
    m_overwriteExisting = overwriteExisting;
    
    // create index for toID
    final IndexFeaturesVisitor visitor = new IndexFeaturesVisitor( toID );
    m_list.accept( visitor );
    m_idHash = visitor.getIndex();
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    final Object fromID = f.getProperty( m_fromID );
    
    final Feature existingFeature = (Feature)m_idHash.get( fromID );
    
    if( existingFeature != null && !m_overwriteExisting )
      return true;
    
    final String fid = fromID == null ? ( m_featureType.getName() + "_" + m_id++ ) : fromID.toString();
    final Feature feature = FeatureFactory.createDefaultFeature( fid, m_featureType, false );
    FeatureHelper.copyProperties( f, feature, m_propertyMap );
    m_list.add( feature );
    
    return true;
  }
}
