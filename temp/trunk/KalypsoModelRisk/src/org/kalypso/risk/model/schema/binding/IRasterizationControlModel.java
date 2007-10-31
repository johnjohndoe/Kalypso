package org.kalypso.risk.model.schema.binding;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;

public interface IRasterizationControlModel extends IModel
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "RasterizationControlModel" );

  public QName PROPERTY_LANDUSE_CLASS_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "landuseClassMember" );

  public QName PROPERTY_ASSET_VALUE_CLASS_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "assetValueClassMember" );

  public QName PROPERTY_ADMINISTRATION_UNIT_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "administrationUnitMember" );

  public QName PROPERTY_DAMAGE_FUNCTION_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "damageFunctionMember" );

  public List<ILanduseClass> getLanduseClassesList( );

  public List<IAssetValueClass> getAssetValueClassesList( );

  public ILanduseClass createNewLanduseClass( );
}
