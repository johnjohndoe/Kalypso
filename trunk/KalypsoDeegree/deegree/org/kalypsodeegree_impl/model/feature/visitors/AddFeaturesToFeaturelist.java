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
 * F�gt neue Features in eine
 * {@link org.kalypsodeegree.model.feature.FeatureList}ein. Dabei wird f�r
 * jedes besuchte Feature ein neues (Default) Feature erzeugt in welches Werte
 * aus dem besuchten Feature via
 * {@link org.kalypsodeegree_impl.model.feature.FeatureHelper#copyProperties(Feature, Feature, Properties)}
 * �bertragen werden.
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

  private final String m_fid;

  private Map m_fidHash;

  private static final String REPLACE_COUNT = "${count}";

  /**
   * @param fid
   *          Anhand dieses Pattern werden die neuen FeatureIds erzeugt. Es wird
   *          ein Pattern-Replace mit folgenden Variablen erzeugt:
   * <ul>
   * <li>fromID</li>
   * <li>toID</li>
   * <li>fID</li>
   * <li>count</li>
   * </ul>
   */
  public AddFeaturesToFeaturelist( final FeatureList list, final Properties propertyMap,
      final FeatureType featureType, final String fromID, final String toID,
      final boolean overwriteExisting, final String fid )
  {
    m_list = list;
    m_featureType = featureType;
    m_propertyMap = propertyMap;
    m_fromID = fromID;
    m_overwriteExisting = overwriteExisting;
    m_fid = fid;
    
    // create index for toID
    final IndexFeaturesVisitor visitor = new IndexFeaturesVisitor( toID );
    m_list.accept( visitor );
    m_idHash = visitor.getIndex();

    final IndexFeaturesVisitor fidVisitor = new IndexFeaturesVisitor( null );
    m_list.accept( fidVisitor );
    m_fidHash = fidVisitor.getIndex();
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

    final String fid = createID( existingFeature, fromID );
    final Feature feature = FeatureFactory.createDefaultFeature( fid, m_featureType, false );
    FeatureHelper.copyProperties( f, feature, m_propertyMap );
    m_list.add( feature );
    
    // den fid-hash aktuell halten
    m_fidHash.put( feature.getId(), feature );

    return true;
  }

  private String createID( final Feature feature, final Object fromID )
  {
    final String oldFid = feature == null ? "<none>" : feature.getId();
    
    String fidhelp = m_fid; 
    fidhelp = fidhelp.replaceAll( "\\Q${fromID}\\E" , fromID.toString() );
//    fidhelp = fidhelp.replaceAll( "\\Q${toID}\\E" , m_fromID );
    fidhelp = fidhelp.replaceAll( "\\Q${fID}\\E", oldFid );
    if( fidhelp.indexOf( REPLACE_COUNT ) == -1 )
      fidhelp += REPLACE_COUNT;
    
    int count = -1;
    while( true )
    {
      final String replace = count == -1 ? "" : ( "" + count );
      final String fid = fidhelp.replaceAll( "\\Q" + REPLACE_COUNT + "\\E", replace );

      if( m_fidHash.get( fid ) == null )
        return fid;

      count++;
    }
  }
}
