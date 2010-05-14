package org.kalypso.model.flood.ui.map.operations;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.core.util.pool.PoolableObjectType;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IRunoffEvent;
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

/**
 * @author Gernot Belger
 */
public final class RemoveEventOperation implements ICoreRunnableWithProgress
{
  private final Object[] m_treeSelection;

  protected final SzenarioDataProvider m_provider;

  private final IKalypsoCascadingTheme m_wspThemes;

  public RemoveEventOperation( final Object[] treeSelection, final SzenarioDataProvider provider, final IKalypsoCascadingTheme wspThemes )
  {
    m_treeSelection = treeSelection;
    m_provider = provider;
    m_wspThemes = wspThemes;
  }

  public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    try
    {
      for( final Object element : m_treeSelection )
      {
        final Feature featureToRemove = (Feature) element;

        /* Delete associated themes */
        final IRunoffEvent runoffEvent = (IRunoffEvent) featureToRemove.getAdapter( IRunoffEvent.class );
        if( runoffEvent != null )
        {
          /* Delete themes from map */
          deleteThemes( m_wspThemes, runoffEvent );

          /* Delete underlying grid files */
          final ICoverageCollection resultCoverages = runoffEvent.getResultCoverages();

          final Display display = PlatformUI.getWorkbench().getDisplay();
          display.asyncExec( new Runnable()
          {
            public void run( )
            {
              FloodModelHelper.removeResultCoverages( m_provider, resultCoverages );
            }
          } );

          /* Delete event folder */
          final IFolder eventFolder = EventManagementWidget.getEventFolder( runoffEvent );
          eventFolder.delete( true, new NullProgressMonitor() );
        }

        /* Delete coverage from collection */
        // final Feature parentFeature = featureToRemove.getParent();
        // final IRelationType pt = featureToRemove.getParentRelation();
        final CommandableWorkspace workspace = m_provider.getCommandableWorkSpace( IFloodModel.class );

        final DeleteFeatureCommand command = new DeleteFeatureCommand( featureToRemove );

        workspace.postCommand( command );
      }

      /*
       * Save model and map, as undo is not possible here and the user should not be able to 'verwerfen' the changes
       */
      m_provider.saveModel( IFloodModel.class, new SubProgressMonitor( monitor, 1 ) );
    }
    catch( final Exception e )
    {
      if( e instanceof CoreException )
        throw (CoreException) e;

      throw new InvocationTargetException( e );
    }

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
              final Feature parent = feature.getParent().getParent();
              if( parent != null )
              {
                if( parent.getId().equals( event.getGmlID() ) )
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