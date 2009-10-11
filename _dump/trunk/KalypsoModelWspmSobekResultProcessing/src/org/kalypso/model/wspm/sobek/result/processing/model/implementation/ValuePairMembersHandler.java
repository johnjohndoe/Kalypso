package org.kalypso.model.wspm.sobek.result.processing.model.implementation;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import javax.xml.namespace.QName;

import org.kalypso.model.wspm.sobek.result.processing.model.IValuePairMember;
import org.kalypso.model.wspm.sobek.result.processing.model.IValuePairMembers;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;

public class ValuePairMembersHandler extends AbstractListWrapper implements IValuePairMembers, List
{

  public ValuePairMembersHandler( final FeatureList list )
  {
    super( list );
  }

  public IValuePairMember[] getValuePairs( )
  {
    final Set<IValuePairMember> members = new LinkedHashSet<IValuePairMember>();

    for( final Object obj : this )
    {
      if( !(obj instanceof Feature) )
        continue;

      members.add( new ValuePairMemberHandler( (Feature) obj ) );
    }

    return members.toArray( new IValuePairMember[] {} );
  }

  @Override
  public Feature addNew( final QName newChildType )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public Feature addNew( final QName newChildType, final String newFeatureId )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public <T extends Feature> T addNew( final QName newChildType, final Class<T> classToAdapt )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public <T extends Feature> T addNew( final QName newChildType, final String newFeatureId, final Class<T> classToAdapt )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public <T extends Feature> boolean addRef( final T toAdd ) throws IllegalArgumentException
  {
    // TODO Auto-generated method stub
    return false;
  }

  @Override
  public Feature insertNew( final int index, final QName newChildType )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public Feature insertNew( final int index, final QName newChildType, final String newFeatureId )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public <T extends Feature> T insertNew( final int index, final QName newChildType, final Class<T> classToAdapt )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public <T extends Feature> T insertNew( final int index, final QName newChildType, final String newFeatureId, final Class<T> classToAdapt )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public <T extends Feature> T insertNew( final int index, final QName newChildType, final String newFeatureId, final Class<T> classToAdapt, final Object[] properties )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public <T extends Feature> boolean insertRef( final int index, final T toAdd ) throws IllegalArgumentException
  {
    // TODO Auto-generated method stub
    return false;
  }

  @Override
  public List<Feature> queryIntersectResolve( final GM_Envelope env, final List<Feature> result )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public List<Feature> queryIntersectResolve( final GM_Point point, final List<Feature> result )
  {
    // TODO Auto-generated method stub
    return null;
  }

}
