package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

public interface IAssetValueClass extends IFeatureWrapper2
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "AssetValueClass" );

  public QName PROP_ASSET_VALUE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "value" );

  public QName PROP_NAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "name" );

  public QName PROP_DESCRIPTION = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "description" );

  public void setName( final String name );

  public void setDescription( final String description );

  public void setAssetValue( final Double assetValue );

  public String getName( );

  public String getDescription( );

  public Double getAssetValue( );

}
