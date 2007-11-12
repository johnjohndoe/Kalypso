package org.kalypso.model.flood.ui.map.operations;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.ui.map.EventManagementWidget;
import org.kalypso.ogc.gml.AbstractCascadingLayerTheme;
import org.kalypso.ogc.gml.GisTemplateUserStyle;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public final class RemoveEventOperation implements ICoreRunnableWithProgress
{
  private final Object[] m_treeSelection;

  private final SzenarioDataProvider m_provider;

  private final AbstractCascadingLayerTheme m_wspThemes;

  public RemoveEventOperation( final Object[] treeSelection, final SzenarioDataProvider provider, final AbstractCascadingLayerTheme wspThemes )
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

          /* Delete event folder */
          final IFolder eventFolder = EventManagementWidget.getEventFolder( runoffEvent );
          eventFolder.delete( true, new NullProgressMonitor() );
        }

        /* Delete coverage from collection */
        final Feature parentFeature = featureToRemove.getParent();
        final IRelationType pt = featureToRemove.getParentRelation();

        final CommandableWorkspace workspace = m_provider.getCommandableWorkSpace( IFloodModel.class );

        final DeleteFeatureCommand command = new DeleteFeatureCommand( workspace, parentFeature, pt, featureToRemove );
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

  private void deleteThemes( final AbstractCascadingLayerTheme wspThemes, final IRunoffEvent event )
  {
    final IKalypsoTheme[] allThemes = wspThemes.getAllThemes();
    for( final IKalypsoTheme kalypsoTheme : allThemes )
    {
      if( kalypsoTheme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme) kalypsoTheme;
        final UserStyle[] styles = featureTheme.getStyles();
        for( final UserStyle userStyle : styles )
        {
          if( userStyle instanceof GisTemplateUserStyle )
          {
            final GisTemplateUserStyle pooledUserStyle = (GisTemplateUserStyle) userStyle;
            final PoolableObjectType poolKey = pooledUserStyle.getPoolKey();

            final String styleLocationForEvent = AddEventOperation.styleLocationForEvent( event );

            if( poolKey.getLocation().equals( styleLocationForEvent ) )
            {
              wspThemes.removeTheme( kalypsoTheme );
              break;
            }
          }
        }
      }
    }
  }
}