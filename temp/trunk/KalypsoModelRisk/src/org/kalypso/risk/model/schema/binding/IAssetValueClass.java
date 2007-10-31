package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;

public interface IAssetValueClass
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "AssetValueClass" );

  public QName PROP_LANDUSE_CLASS_LINK = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "landuseClassLink" );

  public QName PROP_ADMINISTRATION_UNIT_LINK = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "administrationUnitLink" );

  public QName PROP_ASSET_VALUE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "value" );

  public Double getAssetValue( );

  public String getLanduseClassGmlID( );

  public String getAdministrationUnitGmlID( );
}
