package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Surface;

public interface ILandusePolygon extends IFeatureWrapper2
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_LANDUSE_VECTOR_MODEL, "Polygon" );

  public QName QNAME_PROPERTY_GEOMETRY = new QName( KalypsoRiskSchemaCatalog.NS_LANDUSE_VECTOR_MODEL, "polygonGeometry" );

  public QName QNAME_PROPERTY_SLDSTYLE = new QName( KalypsoRiskSchemaCatalog.NS_LANDUSE_VECTOR_MODEL, "sldStyle" );

  void setGeometry( final GM_Surface< ? > surface );

  void setStyleType( final String styleType );
}
