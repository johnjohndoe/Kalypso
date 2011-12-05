package org.kalypso.risk.model.schema.binding;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

public class DamageFunction extends AbstractFeatureBinder implements IDamageFunction
{
  public DamageFunction( final Feature featureToBind )
  {
    super( featureToBind, IDamageFunction.QNAME );
  }

  @Override
  public String getFunction( )
  {
    return (String) getFeature().getProperty( IDamageFunction.PROP_FUNCTION );
  }

  @Override
  public String getName( )
  {
    return (String) getFeature().getProperty( IDamageFunction.PROP_NAME );
  }

  @Override
  public String getDescription( )
  {
    return (String) getFeature().getProperty( IDamageFunction.PROP_DESCRIPTION );
  }

  @Override
  public void setFunction( final String function )
  {
    getFeature().setProperty( IDamageFunction.PROP_FUNCTION, function );
  }

  @Override
  public void setName( final String name )
  {
    getFeature().setProperty( IDamageFunction.PROP_NAME, name );
  }

  @Override
  public void setDescription( final String desc )
  {
    getFeature().setProperty( IDamageFunction.PROP_DESCRIPTION, desc );
  }

}
