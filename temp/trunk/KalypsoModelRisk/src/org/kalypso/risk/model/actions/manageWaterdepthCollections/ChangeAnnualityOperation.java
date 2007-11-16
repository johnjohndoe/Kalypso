package org.kalypso.risk.model.actions.manageWaterdepthCollections;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.ogc.gml.AbstractCascadingLayerTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

public class ChangeAnnualityOperation implements ICoreRunnableWithProgress
{
  private final IRasterDataModel m_model;

  private final AbstractCascadingLayerTheme m_wspThemes;

  private final SzenarioDataProvider m_provider;

  private final int m_newReturnPeriod;

  private final IAnnualCoverageCollection m_coverageCollection;

  public ChangeAnnualityOperation( final Object selection, final int returnPeriod, final IRasterDataModel model, final AbstractCascadingLayerTheme wspThemes, final SzenarioDataProvider provider )
  {
    m_coverageCollection = (IAnnualCoverageCollection) ((Feature) selection).getAdapter( IAnnualCoverageCollection.class );
    m_newReturnPeriod = returnPeriod;
    m_model = model;
    m_wspThemes = wspThemes;
    m_provider = provider;
  }

  public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    try
    {
      monitor.beginTask( "Ereignis hinzuf?gen", 7 );
      final int oldReturnPeriod = m_coverageCollection.getReturnPeriod();
      m_coverageCollection.setReturnPeriod( m_newReturnPeriod );
      m_coverageCollection.setName( "HQ " + m_newReturnPeriod );
      updateTheme( m_wspThemes, oldReturnPeriod );

      final GMLWorkspace workspace = m_model.getWrappedFeature().getWorkspace();
      workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, m_coverageCollection.getWrappedFeature().getParent(), new Feature[] { m_coverageCollection.getWrappedFeature() }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      m_provider.postCommand( IRasterDataModel.class, new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$
      m_provider.saveModel( IRasterDataModel.class, new SubProgressMonitor( monitor, 1 ) );
      return Status.OK_STATUS;
    }
    catch( final IOException e )
    {
      throw new InvocationTargetException( e );
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

  protected void updateTheme( final AbstractCascadingLayerTheme parentKalypsoTheme, final int oldReturnPeriod ) throws Exception
  {
    final IKalypsoTheme[] themes = parentKalypsoTheme.getAllThemes();
    for( int i = 0; i < themes.length; i++ )
      if( themes[i].getName().equals( "HQ " + oldReturnPeriod ) )
      {
        themes[i].setName( "HQ " + m_newReturnPeriod );
        break;
      }
  }
}
