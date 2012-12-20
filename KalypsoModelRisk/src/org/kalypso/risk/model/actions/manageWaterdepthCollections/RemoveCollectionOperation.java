package org.kalypso.risk.model.actions.manageWaterdepthCollections;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.gml.ui.coverage.CoverageManagementHelper;
import org.kalypso.ogc.gml.IKalypsoCascadingTheme;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 */
public final class RemoveCollectionOperation implements ICoreRunnableWithProgress
{
  private final Object[] m_treeSelection;

  private final IScenarioDataProvider m_provider;

  private final IKalypsoCascadingTheme m_wspThemes;

  public RemoveCollectionOperation( final Object[] treeSelection, final IScenarioDataProvider provider, final IKalypsoCascadingTheme wspThemes )
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
      // FIXME: also delete existing result
      // - find corresponding existing result -> the specific damage needs a name on its feature, same as the name of
      // this feature?
      // - find themes of this result
      // - delete theme, result and its coverages
      // REMARK: there is currently no way to get rid of old results; they will not be deleted once the model is
      // recalulated; only results with the same name get incidently overwritten

      // FIXME: also delete risk-zones; they do not correspond to the input any more

      for( final Object element : m_treeSelection )
      {
        final IAnnualCoverageCollection runoffEvent = (IAnnualCoverageCollection) element;

        /* Delete associated themes */
        if( runoffEvent != null )
        {
          /* Delete themes from map */
          deleteThemes( m_wspThemes, runoffEvent );

          // TODO: delete result themes as well
        }

        /* Delete underlying file */
        final IFeatureBindingCollection<ICoverage> coverages = runoffEvent.getCoverages();
        for( final ICoverage coverage : coverages )
          CoverageManagementHelper.deleteRangeSetFile( coverage );

        /* Delete coverage from collection */
        final DeleteFeatureCommand command = new DeleteFeatureCommand( runoffEvent );
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
        // FIXME: should use property on the theme instead
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