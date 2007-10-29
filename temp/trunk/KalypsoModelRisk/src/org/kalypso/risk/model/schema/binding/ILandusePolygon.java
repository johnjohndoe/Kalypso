package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;

public interface ILandusePolygon extends IFeatureWrapper2
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_LANDUSE_VECTOR_MODEL, "Polygon" );

  public QName PROPERTY_GEOMETRY = new QName( KalypsoRiskSchemaCatalog.NS_LANDUSE_VECTOR_MODEL, "polygonGeometry" );

  public QName PROPERTY_LANDUSE_CLASS = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "landuseClassMember" );

  public QName PROPERTY_SLDSTYLE = new QName( KalypsoRiskSchemaCatalog.NS_LANDUSE_VECTOR_MODEL, "sldStyle" );

  public QName PROPERTY_ORDNUMBER = new QName( KalypsoRiskSchemaCatalog.NS_LANDUSE_VECTOR_MODEL, "clsOrdinalNumber" );

  public void setGeometry( final GM_Surface< ? > surface );

  public void setStyleType( final String styleType );

  public String getStyleType( );

  public void setLanduseClass( final Feature landuseClassFeature );

  public int getLanduseClassOrdinalNumber( );

  public boolean contains( final GM_Position position );
}
