package org.kalypso.model.flood.ui.map.operations;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.core.util.pool.PoolableObjectType;
import org.kalypso.model.flood.KalypsoModelFloodPlugin;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.i18n.Messages;
import org.kalypso.model.flood.ui.map.EventManagementWidget;
import org.kalypso.model.flood.util.FloodModelHelper;
import org.kalypso.ogc.gml.GisTemplateUserStyle;
import org.kalypso.ogc.gml.IKalypsoCascadingTheme;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoStyle;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 */
public final class RemoveEventOperation implements ICoreRunnableWithProgress
{
  private final Object[] m_treeSelection;

  protected final IScenarioDataProvider m_provider;

  private final IKalypsoCascadingTheme m_wspThemes;

  public RemoveEventOperation( final Object[] treeSelection, final IScenarioDataProvider provider, final IKalypsoCascadingTheme wspThemes )
  {
    m_treeSelection = treeSelection;
    m_provider = provider;
    m_wspThemes = wspThemes;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    monitor.beginTask( Messages.getString("RemoveEventOperation_0"), m_treeSelection.length + 10 ); //$NON-NLS-1$

    final Collection<IStatus> removeResults = new ArrayList<>();
    try
    {
      for( final Object element : m_treeSelection )
      {
        final Feature featureToRemove = (Feature) element;
        monitor.subTask( String.format( String.format( Messages.getString("RemoveEventOperation_1"), featureToRemove.getName() ) ) ); //$NON-NLS-1$

        final IStatus removeResult = removeEvent( featureToRemove, new SubProgressMonitor( monitor, 1, SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK ) );
        if( !removeResult.isOK() )
          removeResults.add( removeResult );
      }

      /* Save model, as undo is not possible here and the user should not be able to revert the changes */
      monitor.subTask( Messages.getString("RemoveEventOperation_2") ); //$NON-NLS-1$
      m_provider.saveModel( IFloodModel.class.getName(), new SubProgressMonitor( monitor, 10 ) );
    }
    catch( final Exception e )
    {
      if( e instanceof CoreException )
        throw (CoreException) e;

      throw new InvocationTargetException( e );
    }

    if( removeResults.size() == 0 )
      return Status.OK_STATUS;

    final IStatus[] children = removeResults.toArray( new IStatus[removeResults.size()] );
    return new MultiStatus( KalypsoModelFloodPlugin.PLUGIN_ID, 0, children, Messages.getString("RemoveEventOperation_3"), null ); //$NON-NLS-1$
  }

  private IStatus removeEvent( final Feature featureToRemove, final IProgressMonitor monitor ) throws CoreException, Exception
  {
    final String msg = String.format( Messages.getString("RemoveEventOperation_4"), featureToRemove.getName() ); //$NON-NLS-1$
    monitor.beginTask( msg, 100 );

    final IRunoffEvent runoffEvent = (IRunoffEvent) featureToRemove.getAdapter( IRunoffEvent.class );
    IStatus removeResult = null;
    if( runoffEvent != null )
    {
      monitor.subTask( Messages.getString("RemoveEventOperation_5") ); //$NON-NLS-1$
      deleteThemes( m_wspThemes, runoffEvent );
      monitor.worked( 10 );

      /* Delete underlying tin files */
      monitor.subTask( Messages.getString("RemoveEventOperation_6") ); //$NON-NLS-1$
      final ICoverageCollection resultCoverages = runoffEvent.getResultCoverages();
      removeResult = FloodModelHelper.removeResultCoverages( m_provider, resultCoverages );
      monitor.worked( 60 );

      /* Delete event folder */
      monitor.subTask( Messages.getString("RemoveEventOperation_7") ); //$NON-NLS-1$
      final IFolder eventFolder = EventManagementWidget.getEventFolder( runoffEvent );
      eventFolder.delete( true, new SubProgressMonitor( monitor, 20 ) );
    }

    /* Delete coverage from collection */
    monitor.subTask( Messages.getString("RemoveEventOperation_8") ); //$NON-NLS-1$
    final CommandableWorkspace workspace = m_provider.getCommandableWorkSpace( IFloodModel.class.getName() );
    final DeleteFeatureCommand command = new DeleteFeatureCommand( featureToRemove );
    workspace.postCommand( command );
    monitor.worked( 10 );

    // TODO: Probably we need a more sophisticated error handling here
    if( removeResult != null )
      return removeResult;
    return Status.OK_STATUS;
  }

  private void deleteThemes( final IKalypsoCascadingTheme wspThemes, final IRunoffEvent event )
  {
    final IKalypsoTheme[] allThemes = wspThemes.getAllThemes();
    for( final IKalypsoTheme kalypsoTheme : allThemes )
    {
      if( kalypsoTheme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme) kalypsoTheme;
        final IKalypsoStyle[] styles = featureTheme.getStyles();
        for( final IKalypsoStyle style : styles )
        {
          if( style instanceof GisTemplateUserStyle )
          {
            final GisTemplateUserStyle pooledUserStyle = (GisTemplateUserStyle) style;
            final PoolableObjectType poolKey = pooledUserStyle.getPoolKey();

            final String styleLocationForEventWsp = AddEventOperation.styleLocationForEventWsp( event );

            if( poolKey.getLocation().equals( styleLocationForEventWsp ) )
            {
              wspThemes.removeTheme( kalypsoTheme );
              break;
            }
          }
        }
        // check for result coverages
        final FeatureList featureList = featureTheme.getFeatureList();
        if( featureList != null )
        {
          for( final Object object : featureList )
          {
            if( object instanceof Feature )
            {
              final Feature feature = (Feature) object;

              // the papa papa of the coverage is the event
              final Feature parent = feature.getOwner().getOwner();
              if( parent != null )
              {
                if( parent.getId().equals( event.getId() ) )
                {
                  wspThemes.removeTheme( kalypsoTheme );
                }
              }
            }
          }
        }
      }
    }
  }
}
