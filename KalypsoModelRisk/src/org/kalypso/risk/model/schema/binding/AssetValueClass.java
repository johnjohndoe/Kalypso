package org.kalypso.risk.model.schema.binding;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

public class AssetValueClass extends Feature_Impl implements IAssetValueClass
{
  

  public AssetValueClass( Object parent, IRelationType parentRelation, IFeatureType ft, String id, Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public Double getAssetValue( )
  {
    final Object property = getProperty( IAssetValueClass.PROP_ASSET_VALUE );
    if( property != null )
      return (Double) property;
    return Double.NaN;
  }

  @Override
  public String getName( )
  {
    return (String) getProperty( IAssetValueClass.PROP_NAME );
  }

  @Override
  public String getDescription( )
  {
    return (String) getProperty( IAssetValueClass.PROP_DESCRIPTION );
  }

  @Override
  public void setAssetValue( final Double assetValue )
  {
    setProperty( IAssetValueClass.PROP_ASSET_VALUE, assetValue );
  }

  @Override
  public void setDescription( final String desc )
  {
    setProperty( IAssetValueClass.PROP_DESCRIPTION, desc );
  }

  @Override
  public void setName( final String name )
  {
    setProperty( IAssetValueClass.PROP_NAME, name );
  }

}
