package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

public interface IRasterDataModel extends IModel
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTER_DATA_MODEL, "RasterDataModel" );

  public QName PROPERTY_WATERLEVEL_COVERAGE_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_RASTER_DATA_MODEL, "waterlevelCoverageCollection" );

  public QName PROPERTY_SPECIFIC_DAMAGE_COVERAGE_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_RASTER_DATA_MODEL, "specificDamageCoverageCollection" );
  
  public QName PROPERTY_ANNUAL_COVERAGE_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTER_DATA_MODEL, "annualCoverageMember" );

  public QName PROPERTY_LANDUSE_COVERAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTER_DATA_MODEL, "landuseCoverage" );

  public QName PROPERTY_RISK_ZONES_COVERAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTER_DATA_MODEL, "riskZonesCoverage" );

  public IFeatureWrapperCollection<IAnnualCoverageCollection> getWaterlevelCoverageCollection( );

  public IFeatureWrapperCollection<IAnnualCoverageCollection> getSpecificDamageCoverageCollection( );

  public ICoverageCollection getLanduseCoverage( );

  public ICoverageCollection getRiskZonesCoverage( );
}
