package org.kalypso.risk.model.actions.manageWaterdepthCollections;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.ogc.gml.IKalypsoCascadingTheme;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

public class ChangeAnnualityOperation implements ICoreRunnableWithProgress
{
  private final IRasterDataModel m_model;

  private final IKalypsoCascadingTheme m_wspThemes;

  private final IScenarioDataProvider m_provider;

  private final int m_newReturnPeriod;

  private final IAnnualCoverageCollection m_coverageCollection;

  public ChangeAnnualityOperation( final IAnnualCoverageCollection selection, final int returnPeriod, final IRasterDataModel model, final IKalypsoCascadingTheme wspThemes, final IScenarioDataProvider provider )
  {
    m_coverageCollection = selection;
    m_newReturnPeriod = returnPeriod;
    m_model = model;
    m_wspThemes = wspThemes;
    m_provider = provider;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    try
    {
      monitor.beginTask( Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.ChangeAnnualityOperation.0" ), 7 ); //$NON-NLS-1$
      m_coverageCollection.setReturnPeriod( m_newReturnPeriod );
      m_coverageCollection.setName( "HQ " + m_newReturnPeriod ); //$NON-NLS-1$

      RiskModelHelper.addEventThemes( m_wspThemes, m_coverageCollection );

      final GMLWorkspace workspace = m_model.getWorkspace();
      workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, m_coverageCollection.getOwner(), new Feature[] { m_coverageCollection }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
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
}
