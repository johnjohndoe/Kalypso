package org.kalypso.risk.model.actions.manageWaterdepthCollections;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 */
public final class AddCollectionOperation implements ICoreRunnableWithProgress
{
  private final IRasterDataModel m_model;

  private final String m_eventName;

  private final IScenarioDataProvider m_provider;

  private final int m_returnPeriod;

  private Feature m_newFeature;

  public AddCollectionOperation( final String eventName, final int returnPeriod, final IRasterDataModel model, final IScenarioDataProvider provider )
  {
    m_eventName = eventName;
    m_returnPeriod = returnPeriod;
    m_model = model;
    m_provider = provider;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    try
    {
      monitor.beginTask( Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.AddCollectionOperation.0" ), 7 ); //$NON-NLS-1$

      /* Create a unique name */
      final IFeatureBindingCollection<IAnnualCoverageCollection> waterlevelCoverageCollection = m_model.getWaterlevelCoverageCollection();

      ProgressUtilities.worked( monitor, 1 );

      /* Add new feature */
      final IAnnualCoverageCollection newCoverageCollection = waterlevelCoverageCollection.addNew( IAnnualCoverageCollection.QNAME );
      newCoverageCollection.setName( m_eventName );
      newCoverageCollection.setReturnPeriod( m_returnPeriod );
      m_newFeature = newCoverageCollection;

      ProgressUtilities.worked( monitor, 1 );

      /*
       * Save model and map, as undo is not possible here and the user should not be able to 'verwerfen' the changes
       */
      final GMLWorkspace workspace = m_model.getWorkspace();
      workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, newCoverageCollection.getOwner(), new Feature[] { newCoverageCollection }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      m_provider.postCommand( IRasterDataModel.class.getName(), new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$
      m_provider.saveModel( IRasterDataModel.class.getName(), new SubProgressMonitor( monitor, 1 ) );

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      throw new InvocationTargetException( e );
    }
    finally
    {
      monitor.done();
    }
  }

  public Feature getNewFeature( )
  {
    return m_newFeature;
  }

}