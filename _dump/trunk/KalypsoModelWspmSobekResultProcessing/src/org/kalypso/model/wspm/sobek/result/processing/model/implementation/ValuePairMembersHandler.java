package org.kalypso.model.wspm.sobek.result.processing.model.implementation;

import java.util.LinkedHashSet;
import java.util.Set;

import org.kalypso.model.wspm.sobek.result.processing.model.IValuePairMember;
import org.kalypso.model.wspm.sobek.result.processing.model.IValuePairMembers;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

public class ValuePairMembersHandler extends AbstractListWrapper implements IValuePairMembers
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
}
