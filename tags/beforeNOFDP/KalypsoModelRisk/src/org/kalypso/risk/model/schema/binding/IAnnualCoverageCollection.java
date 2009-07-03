package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

/**
 * binding interface for grids holding any annual data used by risk modeller.
 * 
 * @author Dejan Antanaskovic
 */
public interface IAnnualCoverageCollection extends ICoverageCollection
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTER_DATA_MODEL, "AnnualCoverage" );

  public QName PROP_COVERAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTER_DATA_MODEL, "coverage" );

  public QName PROP_ANNUALITY = new QName( KalypsoRiskSchemaCatalog.NS_RASTER_DATA_MODEL, "annuality" );

  public Integer getReturnPeriod( );

  public void setReturnPeriod( final Integer value );
}
