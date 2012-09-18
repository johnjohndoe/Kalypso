/**
 *
 */
package org.kalypso.kalypsosimulationmodel.core.roughness;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.kalypso.afgui.model.UnversionedModel;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;

/**
 * @author Patrice Congo
 */
public class RoughnessClsCollection extends UnversionedModel implements IRoughnessClsCollection
{
  private final IFeatureBindingCollection<IRoughnessCls> m_roughnessClasses = new FeatureBindingCollection<>( this, IRoughnessCls.class, WBR_PROP_ROUGHNESS_CLS_MEMBER );

  public RoughnessClsCollection( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public List<IRoughnessCls> selectRoughnessByName( final String nameRegExp )
  {
    final Pattern p = Pattern.compile( nameRegExp );
    final List<IRoughnessCls> rcList = new ArrayList<>( m_roughnessClasses.size() );
    String name;
    Matcher matcher;

    for( final IRoughnessCls rc : m_roughnessClasses )
    {
      name = rc.getName();
      if( name != null )
      {
        matcher = p.matcher( name );
        if( matcher.matches() )
        {
          rcList.add( rc );
        }
      }
    }
    return rcList;
  }

  @Override
  public IFeatureBindingCollection<IRoughnessCls> getRoughnessClasses( )
  {
    return m_roughnessClasses;
  }
}
