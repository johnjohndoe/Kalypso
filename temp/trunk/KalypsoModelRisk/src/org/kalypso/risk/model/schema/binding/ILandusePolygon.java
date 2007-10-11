package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.risk.model.schema.RiskSchemaCatalog;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

public interface ILandusePolygon extends IFeatureWrapper2
{
  public QName QNAME = new QName( RiskSchemaCatalog.NS_LANDUSEVECTORMODEL, "Polygon" );

  public QName QNAME_PROPERTY_GEOMETRY = new QName( RiskSchemaCatalog.NS_LANDUSEVECTORMODEL, "polygonGeometry" );

  public QName QNAME_PROPERTY_SLDSTYLE = new QName( RiskSchemaCatalog.NS_LANDUSEVECTORMODEL, "sldStyle" );
}
