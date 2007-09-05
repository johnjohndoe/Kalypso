package org.kalypso.model.wspm.ui.action;

import java.io.IOException;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
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
      MessageDialog.openWarning( shell, "Karte erzeugen", "Es wurden keine Feature-Listen in der Selektion gefunden." );
      return;
    }

    createAndOpenMap( action, selectedFeatures, shell );
  }

  public static void createAndOpenMap( final IAction action, final Map<Feature, IRelationType> selectedProfiles, final Shell shell )
  {
    final String mapTemplate = createMapTemplate( action, selectedProfiles, shell );
    if( mapTemplate == null )
      return;

    final UIJob uijob = new UIJob( "Open GML Editor" )
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        try
        {
          final IWorkbench workbench = PlatformUI.getWorkbench();

          final IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
          final IWorkbenchPage page = window.getActivePage();

          final IEditorRegistry editorRegistry = workbench.getEditorRegistry();
          final IEditorDescriptor editorDescription = editorRegistry.findEditor( GisMapEditor.ID );
          final IEditorInput input = new StorageEditorInput( new StringStorage( "<unbekannt>.gmt", mapTemplate, null ) );

          page.openEditor( input, editorDescription.getId(), true );
        }
        catch( final Exception e )
        {
          return StatusUtilities.statusFromThrowable( e );
        }

        return Status.OK_STATUS;
      }
    };
    uijob.setUser( true );
    uijob.schedule();
  }

  private static String createMapTemplate( final IAction action, final Map<Feature, IRelationType> selectedProfiles, final Shell shell )
  {
    try
    {
      final Gismapview gismapview = GisTemplateHelper.createGisMapView( selectedProfiles, true );
      final StringWriter stringWriter = new StringWriter();
      GisTemplateHelper.saveGisMapView( gismapview, stringWriter, "UTF8" );
      stringWriter.close();

      return stringWriter.toString();
    }
    catch( final JAXBException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
      ErrorDialog.openError( shell, action.getText(), "Kartenvorlage konnte nicht erzeugt werden", status );
      return null;
    }
    catch( final IOException e )
    {
      // will never happen as we have a string-writer here
      e.printStackTrace();
      return null;
    }
  }

}
