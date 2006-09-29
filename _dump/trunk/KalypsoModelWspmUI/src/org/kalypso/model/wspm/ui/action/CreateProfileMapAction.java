package org.kalypso.model.wspm.ui.action;

import java.io.OutputStreamWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.actions.ActionDelegate;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.gml.WspmReachProfileSegment;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.ui.editor.gmleditor.ui.FeatureAssociationTypeElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

public class CreateProfileMapAction extends ActionDelegate
{
  private ISelection m_selection;

  /**
   * @see org.eclipse.ui.actions.ActionDelegate#runWithEvent(org.eclipse.jface.action.IAction,
   *      org.eclipse.swt.widgets.Event)
   */
  @Override
  public void runWithEvent( final IAction action, final Event event )
  {
    /* retrieve selected profile-collections/string-segment-collections, abort if none */
    final Map<Feature, IRelationType> selectedProfiles = new HashMap<Feature, IRelationType>();
    if( m_selection instanceof IStructuredSelection )
    {
      for( final Object selectedObject : ((IStructuredSelection) m_selection).toList() )
      {
        if( selectedObject instanceof Feature )
          addFeature( selectedProfiles, (Feature) selectedObject );
        else if( selectedObject instanceof FeatureAssociationTypeElement )
        {
          final FeatureAssociationTypeElement fate = (FeatureAssociationTypeElement) selectedObject;
          final Feature parentFeature = fate.getParentFeature();
          
          selectedProfiles.put( parentFeature, fate.getAssociationTypeProperty() );
        }
      }
    }

    final Shell shell = event.display.getActiveShell();

    if( selectedProfiles.size() == 0 )
    {
      MessageDialog.openWarning( shell, "Karte erzeugen", "Es wurden keine Profile-Container in der Selektion gefunden." );
      return;
    }

    /* Create an empty map with the selected layers */
    final InputDialog dialog = new InputDialog( shell, "Karte erzeugen", "Bitte geben Sie den Kartennamen ein:", "Grundkarte", null );
    if( !(dialog.open() == Window.OK) )
      return;

    final WorkspaceJob job = new WorkspaceJob( "Karte wird erzeugt" )
    {
      @Override
      public IStatus runInWorkspace( final IProgressMonitor monitor ) throws CoreException
      {
        final Gismapview gismapview = GisTemplateHelper.createGisMapView( selectedProfiles, true );

        final String mapName = dialog.getValue();
        final Feature firstFeature = selectedProfiles.keySet().iterator().next();
        final URL context = firstFeature.getWorkspace().getContext();
        try
        {
          final String mapFileName = FileUtilities.setSuffix( mapName, "gmt" );
          
          final URL mapUrl = new URL( context, mapFileName );
          final IFile file = ResourceUtilities.findFileFromURL( mapUrl );

          final SetContentHelper helper = new SetContentHelper()
          {
            @Override
            protected void write( final OutputStreamWriter writer ) throws Throwable
            {
              GisTemplateHelper.saveGisMapView( gismapview, writer, writer.getEncoding() );
            }
          };

          helper.setFileContents( file, false, true, monitor );
        }
        catch( final MalformedURLException e )
        {
          final IStatus status = StatusUtilities.statusFromThrowable( e );
          throw new CoreException( status );
        }

        return Status.OK_STATUS;
      }
    };

    job.setUser( true );
    job.schedule();

    // TODO: open map
  }

  private void addFeature( final Map<Feature, IRelationType> selectedProfiles, final Feature feature )
  {
    final IFeatureType featureType = feature.getFeatureType();
    if( GMLSchemaUtilities.substitutes( featureType, WspmProfile.QNAME_PROFILE ) )
    {
      final IRelationType rt = FeatureHelper.findParentRelation( feature );
      if( rt != null )
        selectedProfiles.put( feature.getParent(), rt );
    }
    else if( GMLSchemaUtilities.substitutes( featureType, WspmReachProfileSegment.QNAME_PROFILEREACHSEGMENT ) )
    {
      final Feature profileFeature = new WspmReachProfileSegment( feature ).getProfileMember().getFeature();
      final IRelationType rt = FeatureHelper.findParentRelation( profileFeature );
      if( rt != null )
        selectedProfiles.put( feature.getParent(), rt );
    }
  }

  /**
   * @see org.eclipse.ui.actions.ActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    m_selection = selection;
  }
}
