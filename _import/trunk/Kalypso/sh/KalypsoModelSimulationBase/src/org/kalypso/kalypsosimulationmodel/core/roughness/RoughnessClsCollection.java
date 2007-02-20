/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core.roughness;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelRoughnessConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Patrice Congo
 */
public class RoughnessClsCollection extends FeatureWrapperCollection<IRoughnessCls> implements IRoughnessClsCollection
{

  /**
   * @param featureCol
   * @param fwClass
   */
  public RoughnessClsCollection( Feature featureCol )
  {
    super( featureCol, IRoughnessCls.class, KalypsoModelRoughnessConsts.WBR_PROP_ROUGHNESS_CLS_MEMBER );
  }

  /**
   * @param parentFeature
   * @param propQName
   * @throws IllegalArgumentException
   */
  public RoughnessClsCollection( Feature parentFeature, QName propQName ) throws IllegalArgumentException
  {
    super( parentFeature, propQName, KalypsoModelRoughnessConsts.WBR_PROP_ROUGHNESS_CLS_MEMBER, IRoughnessCls.class );
  }

  public String getName()
  {
    Feature wrappedFeature = getWrappedFeature();
    return Util.getFirstName( wrappedFeature );
  }

  // public IRoughnessCls getRoughnessByURI(String uri) throws IllegalArgumentException
  // {
  // Assert.throwIAEOnNullOrEmpty(uri);
  // String curUri;
  // for(IRoughnessCls rc:this)
  // {
  // curUri=rc.getURI();
  // if(uri!=null)
  // {
  // if(uri.equals(curUri))
  // {
  // return rc;
  // }
  // }
  // }
  //		
  // return null;
  // }

  public List<IRoughnessCls> selectRoughnessByName( String nameRegExp )
  {
    Pattern p = Pattern.compile( nameRegExp );
    List<IRoughnessCls> rcList = new ArrayList<IRoughnessCls>( size() );
    String name;
    Matcher matcher;

    for( IRoughnessCls rc : this )
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
  public void setName( String name ) throws IllegalArgumentException
  {
    name = Assert.throwIAEOnNullOrEmpty( name );
    Feature wrappedFeature = getWrappedFeature();
    FeatureHelper.addProperty( wrappedFeature, KalypsoModelRoughnessConsts.GML_PROP_NAME, Arrays.asList( new String[] { name } ) );
  }

}
