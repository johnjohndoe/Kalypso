package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.risk.model.schema.RiskSchemaCatalog;

public interface ILanduseModel extends IModel
{
  public QName QNAME = new QName( RiskSchemaCatalog.NS_LANDUSEVECTORMODEL, "LanduseModel" );

  public QName PROPERTY_LANDUSE_COLLECTION = new QName( RiskSchemaCatalog.NS_LANDUSEVECTORMODEL, "landusePolygonCollection" );

  public ILandusePolygonCollection getLandusePolygonCollection( );
}
