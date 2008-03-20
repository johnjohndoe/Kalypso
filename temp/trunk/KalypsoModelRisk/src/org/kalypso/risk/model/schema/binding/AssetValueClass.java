package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

public class AssetValueClass extends AbstractFeatureBinder implements IAssetValueClass
{
  public AssetValueClass( final Feature featureToBind )
  {
    this( featureToBind, IAssetValueClass.QNAME );
  }

  public AssetValueClass( final Feature featureToBind, final QName qnameToBind )
  {
    super( featureToBind, qnameToBind );
  }

  public Double getAssetValue( )
  {
    final Object property = getFeature().getProperty( IAssetValueClass.PROP_ASSET_VALUE );
    if( property != null )
      return (Double) property;
    return Double.NaN;
  }

  public void setAssetValue( final Double assetValue )
  {
    getFeature().setProperty( IAssetValueClass.PROP_ASSET_VALUE, assetValue );
  }

  @Override
  public void setDescription( final String desc )
  {
    getFeature().setProperty( IAssetValueClass.PROP_DESCRIPTION, desc );
  }

  @Override
  public void setName( final String name )
  {
    getFeature().setProperty( IAssetValueClass.PROP_NAME, name );
  }

}
