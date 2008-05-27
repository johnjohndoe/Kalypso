/**
 *
 */
package org.kalypso.risk.model.operation;

import java.util.List;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.risk.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

public final class RiskCalcSpecificDamageRunnable implements ICoreRunnableWithProgress
{
  private final IRasterDataModel m_rasterDataModel;

  private final IFolder m_scenarioFolder;

  private final IVectorDataModel m_vectorDataModel;

  private final IRasterizationControlModel m_controlModel;

  public RiskCalcSpecificDamageRunnable( final IRasterizationControlModel controlModel, final IRasterDataModel rasterDataModel, final IVectorDataModel vectorDataModel, final IFolder scenarioFolder )
  {
    m_controlModel = controlModel;
    m_rasterDataModel = rasterDataModel;
    m_vectorDataModel = vectorDataModel;
    m_scenarioFolder = scenarioFolder;
  }

  public IStatus execute( final IProgressMonitor monitor )
  {
    final IFeatureWrapperCollection<IAnnualCoverageCollection> specificDamageCoverageCollection = m_rasterDataModel.getSpecificDamageCoverageCollection();
    final List<ILanduseClass> landuseClassesList = m_controlModel.getLanduseClassesList();

    final IFeatureWrapperCollection<ILandusePolygon> polygonCollection = m_vectorDataModel.getLandusePolygonCollection();

    if( m_rasterDataModel.getWaterlevelCoverageCollection().size() == 0 )
      return StatusUtilities.createErrorStatus( Messages.getString( org.kalypso.risk.Messages.getString( "RiskCalcSpecificDamageRunnable.0" ) ) ); //$NON-NLS-1$

    for( final IAnnualCoverageCollection collection : m_rasterDataModel.getWaterlevelCoverageCollection() )
    {
      final Integer returnPeriod = collection.getReturnPeriod();
      if( returnPeriod == null || returnPeriod <= 0 )
        return StatusUtilities.createErrorStatus( Messages.getString( "DamagePotentialCalculationHandler.18" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    }

    monitor.beginTask( Messages.getString( "DamagePotentialCalculationHandler.9" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
    try
    {
      /* clear existing data */
      specificDamageCoverageCollection.clear();
      for( ILanduseClass landuseClass : landuseClassesList )
        landuseClass.clearStatisticEntries();

      /* loop over all waterdepths */
      for( final IAnnualCoverageCollection srcAnnualCoverages : m_rasterDataModel.getWaterlevelCoverageCollection() )
      {
        monitor.subTask( Messages.getString( "DamagePotentialCalculationHandler.10" ) + srcAnnualCoverages.getReturnPeriod() ); //$NON-NLS-1$

        /* create annual damage coverage collection */
        final IAnnualCoverageCollection dstSpecificDamageCoverages = RiskModelHelper.createSpecificDamageCoverages( m_scenarioFolder, polygonCollection, srcAnnualCoverages, specificDamageCoverageCollection, landuseClassesList );

        /* fireModellEvent to redraw a map */
        final GMLWorkspace workspace = m_rasterDataModel.getFeature().getWorkspace();
        workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, specificDamageCoverageCollection.getFeature(), new Feature[] { dstSpecificDamageCoverages.getFeature() }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      }

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, org.kalypso.risk.Messages.getString( "RiskCalcSpecificDamageRunnable.1" ) ); //$NON-NLS-1$
    }
  }
}