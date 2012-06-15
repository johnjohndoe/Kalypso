package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.afgui.model.IModel;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

public interface IRasterDataModel extends IModel
{
  public static final String MODEL_NAME = "RasterDataModel"; //$NON-NLS-1$

  public static final String MODEL_ID = "org.kalypso.risk.model.schema.binding.IRasterDataModel"; //$NON-NLS-1$

  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTER_DATA_MODEL, "RasterDataModel" ); //$NON-NLS-1$

  public QName PROPERTY_WATERLEVEL_COVERAGE_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_RASTER_DATA_MODEL, "waterlevelCoverageCollection" ); //$NON-NLS-1$

  public QName PROPERTY_SPECIFIC_DAMAGE_COVERAGE_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_RASTER_DATA_MODEL, "specificDamageCoverageCollection" ); //$NON-NLS-1$

  public QName PROPERTY_ANNUAL_COVERAGE_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTER_DATA_MODEL, "annualCoverageMember" ); //$NON-NLS-1$

  public QName PROPERTY_LANDUSE_COVERAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTER_DATA_MODEL, "landuseCoverage" ); //$NON-NLS-1$

  public QName PROPERTY_RISK_ZONES_COVERAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTER_DATA_MODEL, "riskZonesCoverage" ); //$NON-NLS-1$

  public IFeatureBindingCollection<IAnnualCoverageCollection> getWaterlevelCoverageCollection( );

  public IFeatureBindingCollection<IAnnualCoverageCollection> getSpecificDamageCoverageCollection( );

  public ICoverageCollection getLanduseCoverage( );

  public ICoverageCollection getRiskZonesCoverage( );
}
