package org.kalypso.risk.model.schema.binding;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

public class DamageFunction extends Feature_Impl implements IDamageFunction
{

  public DamageFunction( Object parent, IRelationType parentRelation, IFeatureType ft, String id, Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public String getFunction( )
  {
    return (String) getProperty( IDamageFunction.PROP_FUNCTION );
  }

  @Override
  public String getName( )
  {
    return (String) getProperty( IDamageFunction.PROP_NAME );
  }

  @Override
  public String getDescription( )
  {
    return (String) getProperty( IDamageFunction.PROP_DESCRIPTION );
  }

  @Override
  public void setFunction( final String function )
  {
    setProperty( IDamageFunction.PROP_FUNCTION, function );
  }

  @Override
  public void setName( final String name )
  {
    setProperty( IDamageFunction.PROP_NAME, name );
  }

  @Override
  public void setDescription( final String desc )
  {
    setProperty( IDamageFunction.PROP_DESCRIPTION, desc );
  }

}
