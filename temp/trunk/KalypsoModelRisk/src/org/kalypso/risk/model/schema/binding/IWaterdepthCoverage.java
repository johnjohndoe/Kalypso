package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import ogc31.www.opengis.net.gml.RangeSetType;

import org.kalypso.commons.xml.NS;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;

public interface IWaterdepthCoverage extends ICoverage
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_WATERDEPTH_COVERAGE_MODEL, "WaterdepthCoverage" );

  public QName PROP_RECTIFIED_GRID_DOMAIN = new QName( NS.GML3, "rectifiedGridDomain" );

  public QName PROP_RANGE_SET = new QName( NS.GML3, "rangeSet" );

  public QName PROP_RETURN_PERIOD = new QName( KalypsoRiskSchemaCatalog.NS_WATERDEPTH_COVERAGE_MODEL, "returnPeriod" );

  public Integer getReturnPeriod( );

  public void setReturnPeriod( final Integer value );

  public RectifiedGridDomain getGridDomain( );

  public void setGridDomain( final RectifiedGridDomain gridDomain );

  public RangeSetType getRangeSet( );

  public void setRangeSet( final RangeSetType rangeSet );
}
