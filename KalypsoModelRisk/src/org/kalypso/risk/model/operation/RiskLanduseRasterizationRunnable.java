/**
 * 
 */
package org.kalypso.risk.model.operation;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

public final class RiskLanduseRasterizationRunnable implements ICoreRunnableWithProgress
{
  private final IRasterDataModel m_rasterModel;

  private final IVectorDataModel m_vectorDataModel;

  private final IFolder m_scenarioFolder;

  public RiskLanduseRasterizationRunnable( final IRasterDataModel rasterModel, final IVectorDataModel vectorDataModel, final IFolder scenarioFolder )
  {
    m_vectorDataModel = vectorDataModel;
    m_rasterModel = rasterModel;
    m_scenarioFolder = scenarioFolder;
  }

  public IStatus execute( IProgressMonitor monitor )
  {
    monitor.beginTask( "Erstelle Rasterung aus Landnutzungs Vektordaten...", IProgressMonitor.UNKNOWN );

    final IFeatureWrapperCollection<IAnnualCoverageCollection> waterDepthCoverageCollection = m_rasterModel.getWaterlevelCoverageCollection();

    if( waterDepthCoverageCollection.size() == 0 )
      return StatusUtilities.createErrorStatus( "Keine Fliesstiefen Rasterdaten vorhanden. Bitte importieren Sie zuerst die Fliesstiefen." ); //$NON-NLS-1$ //$NON-NLS-2$

    final IAnnualCoverageCollection maxCoveragesCollection = RiskModelHelper.getMaxReturnPeriodCollection( waterDepthCoverageCollection );
    final Integer maxReturnPeriod = maxCoveragesCollection.getReturnPeriod();

    if( maxReturnPeriod == Integer.MIN_VALUE )
      return StatusUtilities.createErrorStatus( "Missing HQ data. No waterlevel data loaded. Please load waterlevel raster data before rasterizing landuse classes." );

    final ICoverageCollection inputCoverages = maxCoveragesCollection;
    final ICoverageCollection outputCoverages = m_rasterModel.getLanduseCoverage();

    final IFeatureWrapperCollection<ILandusePolygon> polygonCollection = m_vectorDataModel.getLandusePolygonCollection();

    // remove existing (invalid) coverages from the model
    m_rasterModel.getRiskZonesCoverage().clear();

    // TODO: delete old landuse coverage (also the files!)
    IStatus rasterStatus = RiskModelHelper.doRasterLanduse( m_scenarioFolder, inputCoverages, outputCoverages, polygonCollection );
    if( !rasterStatus.isOK() )
      return rasterStatus;

    /* fireModellEvent to redraw a map */
    final GMLWorkspace workspace = m_rasterModel.getFeature().getWorkspace();
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, m_rasterModel.getFeature(), new Feature[] { outputCoverages.getFeature() }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    return Status.OK_STATUS;
  }
}