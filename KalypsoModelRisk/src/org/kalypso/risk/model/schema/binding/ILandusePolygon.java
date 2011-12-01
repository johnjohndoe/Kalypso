package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;

public interface ILandusePolygon extends IFeatureWrapper2
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_VECTOR_DATA_MODEL, "Polygon" ); //$NON-NLS-1$

  public QName PROPERTY_GEOMETRY = new QName( KalypsoRiskSchemaCatalog.NS_VECTOR_DATA_MODEL, "polygonGeometry" ); //$NON-NLS-1$

  public QName PROPERTY_LANDUSE_CLASS = new QName( KalypsoRiskSchemaCatalog.NS_VECTOR_DATA_MODEL, "landuseClassLink" ); //$NON-NLS-1$

  public QName PROPERTY_SLDSTYLE = new QName( KalypsoRiskSchemaCatalog.NS_VECTOR_DATA_MODEL, "sldStyle" ); //$NON-NLS-1$

  public QName PROPERTY_ORDNUMBER = new QName( KalypsoRiskSchemaCatalog.NS_VECTOR_DATA_MODEL, "clsOrdinalNumber" ); //$NON-NLS-1$

  public QName PROPERTY_DAMAGE_FUNCTION = new QName( KalypsoRiskSchemaCatalog.NS_VECTOR_DATA_MODEL, "damageFunction" ); //$NON-NLS-1$

  public QName PROPERTY_ASSET_VALUE = new QName( KalypsoRiskSchemaCatalog.NS_VECTOR_DATA_MODEL, "assetValue" ); //$NON-NLS-1$

  public QName PROPERTY_ISURBANTYPE = new QName( KalypsoRiskSchemaCatalog.NS_VECTOR_DATA_MODEL, "isUrbanLanduseType" ); //$NON-NLS-1$

  public void setGeometry( final GM_Surface< ? > surface );

  public GM_Surface< ? > getGeometry( );

  public void setStyleType( final String styleType );

  public String getStyleType( );

  public void setLanduseClass( final Feature landuseClassFeature );

  public Integer getLanduseClassOrdinalNumber( );

  /**
   * calculates the damage value:<br>
   * <code>asset value * damage function( depth )[%] </code>
   */
  public double getDamageValue( final double waterDepth );

  public boolean contains( final GM_Position position );

  public Boolean isUrbanLanduseType( );

  public void updateStatisticsAverageAnnualDamage( final double value );

  public double getStatisticsAverageAnnualDamage( );

  public ILanduseClass getLanduseClass( final IRasterizationControlModel mode );

}
