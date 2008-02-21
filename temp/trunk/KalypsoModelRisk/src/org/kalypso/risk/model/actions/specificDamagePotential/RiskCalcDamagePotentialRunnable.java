/**
 * 
 */
package org.kalypso.risk.model.actions.specificDamagePotential;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.risk.model.actions.dataImport.waterdepth.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

public final class RiskCalcDamagePotentialRunnable implements ICoreRunnableWithProgress
{
  private final IRasterDataModel m_rasterDataModel;

  private final IFolder m_scenarioFolder;

  private final IVectorDataModel m_vectorDataModel;

  public RiskCalcDamagePotentialRunnable( final IRasterDataModel rasterDataModel, final IVectorDataModel vectorDataModel, final IFolder scenarioFolder )
  {
    m_rasterDataModel = rasterDataModel;
    m_vectorDataModel = vectorDataModel;
    m_scenarioFolder = scenarioFolder;
  }

  public IStatus execute( IProgressMonitor monitor ) throws CoreException, InvocationTargetException, InterruptedException
  {
    final IFeatureWrapperCollection<IAnnualCoverageCollection> specificDamageCoverageCollection = m_rasterDataModel.getSpecificDamageCoverageCollection();
    final IFeatureWrapperCollection<ILandusePolygon> polygonCollection = m_vectorDataModel.getLandusePolygonCollection();

    if( m_rasterDataModel.getWaterlevelCoverageCollection().size() == 0 )
      return StatusUtilities.createErrorStatus( Messages.getString( "DamagePotentialCalculationHandler.6" ) );

    for( final IAnnualCoverageCollection collection : m_rasterDataModel.getWaterlevelCoverageCollection() )
    {
      final Integer returnPeriod = collection.getReturnPeriod();
      if( returnPeriod == null || returnPeriod <= 0 )
        return StatusUtilities.createErrorStatus( Messages.getString( "DamagePotentialCalculationHandler.18" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    }

    monitor.beginTask( Messages.getString( "DamagePotentialCalculationHandler.9" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
    try
    {
      specificDamageCoverageCollection.clear();
      for( final IAnnualCoverageCollection srcAnnualCoverages : m_rasterDataModel.getWaterlevelCoverageCollection() )
      {
        monitor.subTask( Messages.getString( "DamagePotentialCalculationHandler.10" ) + srcAnnualCoverages.getReturnPeriod() ); //$NON-NLS-1$

        /* create annual damage coverage collection */
        final IAnnualCoverageCollection dstAnnualCoverages = RiskModelHelper.createAnnualDamageCoverages( m_scenarioFolder, polygonCollection, srcAnnualCoverages, specificDamageCoverageCollection );

        /* fireModellEvent to redraw a map */
        final GMLWorkspace workspace = m_rasterDataModel.getFeature().getWorkspace();
        workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, specificDamageCoverageCollection.getFeature(), new Feature[] { dstAnnualCoverages.getFeature() }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      }

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new InvocationTargetException( e );
    }
  }
}