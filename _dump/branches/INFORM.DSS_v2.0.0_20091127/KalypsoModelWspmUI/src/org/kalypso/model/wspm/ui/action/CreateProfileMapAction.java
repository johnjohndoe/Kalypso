package org.kalypso.model.wspm.ui.action;

import java.io.IOException;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXBException;

import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionDelegate;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.core.resources.StringStorage;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.editorinput.StorageEditorInput;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.ui.editor.gmleditor.ui.FeatureAssociationTypeElement;
import org.kalypso.ui.editor.mapeditor.GisMapEditor;
import org.kalypsodeegree.model.feature.Feature;

public class CreateProfileMapAction extends ActionDelegate
{
  private IFeatureSelection m_selection;

  /**
   * @see org.eclipse.ui.actions.ActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    m_selection = selection instanceof IFeatureSelection ? (IFeatureSelection) selection : null;

    if( action != null )
      action.setEnabled( m_selection != null );
  }

  /**
   * @see org.eclipse.ui.actions.ActionDelegate#runWithEvent(org.eclipse.jface.action.IAction,
   *      org.eclipse.swt.widgets.Event)
   */
  @Override
  public void runWithEvent( final IAction action, final Event event )
  {
    /* retrieve selected profile-collections, abort if none */
    final Map<Feature, IRelationType> selectedFeatures = new HashMap<Feature, IRelationType>();
    for( final Object selectedObject : m_selection.toList() )
    {
      if( selectedObject instanceof FeatureAssociationTypeElement )
      {
        final FeatureAssociationTypeElement fate = (FeatureAssociationTypeElement) selectedObject;
        final Feature parentFeature = fate.getParentFeature();

        selectedFeatures.put( parentFeature, fate.getAssociationTypeProperty() );
      }
    }

    final Shell shell = event.display.getActiveShell();

    if( selectedFeatures.size() == 0 )
    {
      MessageDialog.openWarning( shell, org.kalypso.model.wspm.ui.i18n.Messages.getString( "org.kalypso.model.wspm.ui.action.CreateProfileMapAction.0" ), org.kalypso.model.wspm.ui.i18n.Messages.getString( "org.kalypso.model.wspm.ui.action.CreateProfileMapAction.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      return;
    }

    final IWorkbenchPart activePart = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActivePart();
    createAndOpenMap( activePart, selectedFeatures );
  }

  public static void createAndOpenMap( final IWorkbenchPart activePart, final Map<Feature, IRelationType> selectedProfiles )
  {

    final UIJob uijob = new UIJob( org.kalypso.model.wspm.ui.i18n.Messages.getString( "org.kalypso.model.wspm.ui.action.CreateProfileMapAction.2" ) ) //$NON-NLS-1$
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        try
        {
          final String title = guessTitle( selectedProfiles.keySet() );
          final String mapTemplate = createMapTemplate( selectedProfiles, title );
          if( mapTemplate == null )
            return Status.OK_STATUS;

          final String storageName = title == null ? "<unbekannt>.gmt" : title + ".gmt"; //$NON-NLS-1$ //$NON-NLS-2$
          final IPath storagePath = guessPath( activePart, storageName );

          final IWorkbenchPartSite activeSite = activePart.getSite();
          final IWorkbenchPage page = activeSite.getPage();
          final IWorkbench workbench = activeSite.getWorkbenchWindow().getWorkbench();

          final IEditorRegistry editorRegistry = workbench.getEditorRegistry();
          final IEditorDescriptor editorDescription = editorRegistry.findEditor( GisMapEditor.ID );

          final IFileEditorInput input = new StorageEditorInput( new StringStorage( mapTemplate, storagePath ) );

          page.openEditor( input, editorDescription.getId(), true );
        }
        catch( final CoreException e )
        {
          final IStatus status = e.getStatus();
          KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
          return status;
        }

        return Status.OK_STATUS;
      }
    };
    uijob.setUser( true );
    uijob.schedule();
  }

  static IPath guessPath( final IWorkbenchPart part, final String storageName ) throws CoreException
  {
    if( part instanceof IEditorPart )
    {
      final IEditorInput editorInput = ((IEditorPart) part).getEditorInput();
      if( editorInput instanceof IStorageEditorInput )
      {
        final IStorage storage = ((IStorageEditorInput) editorInput).getStorage();
        final IPath fullPath = storage.getFullPath();
        if( fullPath != null )
        {
          final IPath parentPath = fullPath.removeLastSegments( 1 );
          return parentPath.append( storageName );
        }
      }
    }

    return null;
  }

  static String guessTitle( final Set<Feature> keySet )
  {
    if( keySet.isEmpty() )
      return null;

    final Feature firstFeature = keySet.iterator().next();
    return firstFeature.getName();
  }

  static String createMapTemplate( final Map<Feature, IRelationType> selectedProfiles, final String title ) throws CoreException
  {
    try
    {
      final Gismapview gismapview = GisTemplateHelper.createGisMapView( selectedProfiles, true );
      if( title != null )
        gismapview.setName( title );
      final StringWriter stringWriter = new StringWriter();
      GisTemplateHelper.saveGisMapView( gismapview, stringWriter, "UTF8" ); //$NON-NLS-1$
      stringWriter.close();

      return stringWriter.toString();
    }
    catch( final JAXBException e )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, org.kalypso.model.wspm.ui.i18n.Messages.getString( "org.kalypso.model.wspm.ui.action.CreateProfileMapAction.4" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
    catch( final IOException e )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, e.getLocalizedMessage(), e );
      throw new CoreException( status );
    }
  }

}
