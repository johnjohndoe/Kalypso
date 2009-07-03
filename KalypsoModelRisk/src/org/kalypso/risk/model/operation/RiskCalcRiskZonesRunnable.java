/**
 * 
 */
package org.kalypso.risk.model.operation;

import java.io.File;
import java.util.Date;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.risk.model.actions.riskZonesCalculation.Messages;
import org.kalypso.risk.model.actions.riskZonesCalculation.RiskZonesGrid;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

public final class RiskCalcRiskZonesRunnable implements ICoreRunnableWithProgress
{
  private final IRasterDataModel m_rasterModel;

  private final IVectorDataModel m_vectorModel;

  private final IRasterizationControlModel m_controlModel;

  private final IFolder m_scenarioFolder;

  public RiskCalcRiskZonesRunnable( IRasterDataModel rasterModel, IVectorDataModel vectorModel, IRasterizationControlModel controlModel, IFolder scenarioFolder )
  {
    m_rasterModel = rasterModel;
    m_controlModel = controlModel;
    m_scenarioFolder = scenarioFolder;
    m_vectorModel = vectorModel;
  }

  public IStatus execute( IProgressMonitor monitor )
  {
    monitor.beginTask( Messages.getString( "RiskZonesCalculationHandler.7" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$

    if( m_rasterModel.getSpecificDamageCoverageCollection().size() < 2 )
      return StatusUtilities.createErrorStatus( Messages.getString( "RiskZonesCalculationHandler.6" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    try
    {
      /* remove existing (invalid) coverages from the model and clean statistic */
      m_rasterModel.getRiskZonesCoverage().clear();
      m_controlModel.resetStatistics();

      final ICoverageCollection outputCoverages = m_rasterModel.getRiskZonesCoverage();

      final IAnnualCoverageCollection maxCoveragesCollection = RiskModelHelper.getMaxReturnPeriodCollection( m_rasterModel.getSpecificDamageCoverageCollection() );
      final ICoverageCollection baseCoverages = maxCoveragesCollection;

      for( int i = 0; i < baseCoverages.size(); i++ )
      {
        final ICoverage srcSpecificDamageCoverage = baseCoverages.get( i );

        final IGeoGrid inputGrid = GeoGridUtilities.toGrid( srcSpecificDamageCoverage );
        final IGeoGrid outputGrid = new RiskZonesGrid( inputGrid, m_rasterModel.getSpecificDamageCoverageCollection(), m_vectorModel.getLandusePolygonCollection(), m_controlModel.getLanduseClassesList(), m_controlModel.getRiskZoneDefinitionsList() );

        // TODO: change name: better: use input name
        final String outputFilePath = "raster/output/RiskZonesCoverage" + i + ".dat"; //$NON-NLS-1$ //$NON-NLS-2$
        final IFile ifile = m_scenarioFolder.getFile( new Path( "models/" + outputFilePath ) ); //$NON-NLS-1$
        final File file = new File( ifile.getRawLocation().toPortableString() );

        final ICoverage coverage = GeoGridUtilities.addCoverage( outputCoverages, outputGrid, file, outputFilePath, "image/bin", new NullProgressMonitor() ); //$NON-NLS-1$

        inputGrid.dispose();

        coverage.setName( "Risikozonen [" + i + "]" );
        // TODO: check for right time zone?
        coverage.setDescription( Messages.getString( "RiskZonesCalculationHandler.9" ) + new Date().toString() ); //$NON-NLS-1$

        /* fireModellEvent to redraw a map */
        final GMLWorkspace workspace = m_rasterModel.getFeature().getWorkspace();
        workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, m_rasterModel.getFeature(), new Feature[] { outputCoverages.getFeature() }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      }

      // statistics...

      // TODO: what gets fixed here? the data should be valid!
      // if not, then there is a general error in the calculation of the values!
      m_controlModel.fixStatisticsForShowingToUser();

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, "Fehler bei Berechnung der Risikozonen" );
    }
  }
}