package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.kalypsosimulationmodel.core.modeling.IColorStyledFeatureWrapper;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;

public interface IRiskZoneDefinition extends IColorStyledFeatureWrapper
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "RiskZoneDefinition" );

  public QName PROP_ORDINAL_NUMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "zoneInternalNumber" );

  public QName PROP_COLOR_STYLE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "colorStyle" );

  public QName PROP_LOWER_BOUNDARY = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "lowerBoundaryValue" );

  public QName PROP_ISURBANTYPE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "isUrbanLanduseType" );

  public QName PROP_NAME = new QName( NS.GML3, "name" );

  public QName PROP_DESCRIPTION = new QName( NS.GML3, "description" );

  public double getLowerBoundary( );

  public void setLowerBoundary( final double value );

  public Boolean isUrbanLanduseType( );
}
