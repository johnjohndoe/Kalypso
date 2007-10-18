package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;

public interface IRasterizationControlModel extends IModel
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "RasterizationControlModel" );

  public QName PROPERTY_LANDUSE_CLASS_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "landuseClassCollection" );

  public ILanduseClassCollection getLanduseClassCollection( );
}
