package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

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

  public String getAdministrationUnitGmlID( )
  {
    final Object property = getFeature().getProperty( IAssetValueClass.PROP_ADMINISTRATION_UNIT_LINK );
    if( property != null && property instanceof XLinkedFeature_Impl )
      return ((XLinkedFeature_Impl) property).getFeatureId();
    return "";
  }

  public Double getAssetValue( )
  {
    final Object property = getFeature().getProperty( IAssetValueClass.PROP_ASSET_VALUE );
    if( property != null )
      return (Double) property;
    return Double.NaN;
  }

  public String getLanduseClassGmlID( )
  {
    final Object property = getFeature().getProperty( IAssetValueClass.PROP_LANDUSE_CLASS_LINK );
    if( property != null && property instanceof XLinkedFeature_Impl )
      return ((XLinkedFeature_Impl) property).getFeatureId();
    return "";
  }

  public void setAssetValue( final Double assetValue )
  {
    getFeature().setProperty( IAssetValueClass.PROP_ASSET_VALUE, assetValue );
  }

}
