package org.kalypso.risk.model.actions.manageWaterdepthCollections;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.ogc.gml.IKalypsoCascadingTheme;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public final class RemoveCollectionOperation implements ICoreRunnableWithProgress
{
  private final Object[] m_treeSelection;

  private final SzenarioDataProvider m_provider;

  private final IKalypsoCascadingTheme m_wspThemes;

  public RemoveCollectionOperation( final Object[] treeSelection, final SzenarioDataProvider provider, final IKalypsoCascadingTheme wspThemes )
  {
    m_treeSelection = treeSelection;
    m_provider = provider;
    m_wspThemes = wspThemes;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    try
    {
      for( final Object element : m_treeSelection )
      {
        final Feature featureToRemove = (Feature) element;

        /* Delete associated themes */
        final IAnnualCoverageCollection runoffEvent = (IAnnualCoverageCollection) featureToRemove.getAdapter( IAnnualCoverageCollection.class );
        if( runoffEvent != null )
        {
          /* Delete themes from map */
          deleteThemes( m_wspThemes, runoffEvent );
        }

        /* Delete coverage from collection */
        final DeleteFeatureCommand command = new DeleteFeatureCommand( featureToRemove );
        final CommandableWorkspace workspace = m_provider.getCommandableWorkSpace( IRasterDataModel.class.getName() );
        workspace.postCommand( command );
      }

      /*
       * Save model and map, as undo is not possible here and the user should not be able to 'verwerfen' the changes
       */
      m_provider.saveModel( IRasterDataModel.class.getName(), new SubProgressMonitor( monitor, 1 ) );
    }
    catch( final Exception e )
    {
      if( e instanceof CoreException )
        throw (CoreException) e;

      throw new InvocationTargetException( e );
    }

    return Status.OK_STATUS;
  }

  private void deleteThemes( final IKalypsoCascadingTheme wspThemes, final IAnnualCoverageCollection event )
  {
    final IKalypsoTheme[] allThemes = wspThemes.getAllThemes();
    IKalypsoTheme themeToRemove = null;
    for( final IKalypsoTheme kalypsoTheme : allThemes )
    {
      if( kalypsoTheme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme) kalypsoTheme;
        if( featureTheme.getName().getKey().equals( "HQ " + event.getReturnPeriod() ) ) //$NON-NLS-1$
        {
          themeToRemove = kalypsoTheme;
          break;
        }
      }
    }
    if( themeToRemove != null )
      wspThemes.removeTheme( themeToRemove );
  }
}