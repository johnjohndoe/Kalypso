package org.kalypso.ogc.gml.filterdialog.actions;

import java.util.ArrayList;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.KalypsoFeatureThemeSelection;
import org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractGisEditorActionDelegate;
import org.kalypso.ui.editor.mapeditor.actiondelegates.WidgetActionPart;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

public class KalypsoOpenFilterDialogActionDelegate extends AbstractGisEditorActionDelegate
{

  IFeatureSelection m_spatialOpSelection = null;

  IStructuredSelection m_ftSelection = null;

  public void run( IAction action )
  {
    Object sGeomOp = null;
    if( m_spatialOpSelection != null )
      sGeomOp = m_spatialOpSelection.getFirstElement();
    final Object sFtOutline = m_ftSelection.getFirstElement();
    Feature fGeom = null;
    if( sGeomOp instanceof Feature )
    {
      fGeom = ((Feature) sGeomOp);
    }
    IFeatureType ft = null;
    IKalypsoFeatureTheme selectedTheme = null;
    FeatureList visableFeatures = null;
    if( sFtOutline instanceof IKalypsoFeatureTheme )
    {
      selectedTheme = ((IKalypsoFeatureTheme) sFtOutline);
      visableFeatures = selectedTheme.getFeatureListVisible( selectedTheme.getBoundingBox() );
      ft = selectedTheme.getFeatureType();
    }

    final WidgetActionPart part = getPart();
    final Shell shell = part == null ? null : part.getSite().getShell();
    final FilterDialog dialog = new FilterDialog( shell, ft, null, null, fGeom, null, true );
    int open = -1;
    if( ft != null )
      open = dialog.open();
    else
      MessageDialog.openError( shell, "Filter-Dialog", "Der gew�hlte FeatureType ist entweder leer oder ung�ltig" );
    if( open == Window.OK )
    {

      MultiStatus multiStatus = null;
      final Filter filter = dialog.getFilter();
      final Feature[] features = (Feature[]) visableFeatures.toArray( new Feature[visableFeatures.size()] );
      final ArrayList<Feature> newSelectedFeatures = new ArrayList<Feature>();
      for( int i = 0; i < features.length; i++ )
      {
        final Feature f = features[i];
        try
        {
          if( filter.evaluate( f ) )
            newSelectedFeatures.add( f );
        }
        catch( FilterEvaluationException e )
        {
          IStatus s = StatusUtilities.statusFromThrowable( e );
          if( multiStatus == null )
            multiStatus = new MultiStatus( KalypsoGisPlugin.getId(), s.getSeverity(), new IStatus[] { s }, e.getMessage(), e );
          multiStatus.merge( s );
          e.printStackTrace();
        }
      }
      if( multiStatus != null )
      {
        MessageDialog.openWarning( shell, "Filter-Dialog-Fehler: sevirity code = " + multiStatus.getSeverity(), multiStatus.getException().getMessage() );
        return;
      }
      // handle the new selection
      final IFeatureSelectionManager selectionManager = selectedTheme.getSelectionManager();
      final EasyFeatureWrapper[] featureWrappers = FeatureSelectionHelper.createEasyWrappers( new KalypsoFeatureThemeSelection( newSelectedFeatures, selectedTheme, selectionManager, null, null ) );
      selectionManager.changeSelection( features, featureWrappers );
    }

  }

  @Override
  public void selectionChanged( IAction action, ISelection selection )
  {
    action.setEnabled( false );

    final WidgetActionPart part = getPart();
    if( part != null )
    {
      final IWorkbenchPart workbenchPart = part.getPart();
      final IContentOutlinePage page = workbenchPart == null ? null : (IContentOutlinePage) workbenchPart.getAdapter( IContentOutlinePage.class );
      if( page != null )
      {
        m_ftSelection = null;
        final Display display = part.getSite().getShell().getDisplay();
        display.syncExec( new Runnable()
        {
          public void run( )
          {
            m_ftSelection = (IStructuredSelection) page.getSelection();
          }
        } );
      }
      if( m_ftSelection != null && !m_ftSelection.isEmpty() )
      {
        if( selection instanceof IFeatureSelection )
        {
          m_spatialOpSelection = (IFeatureSelection) selection;
        }
        action.setEnabled( true );
      }
    }
  }

  /**
   * @see org.kalypso.ui.editor.AbstractGisEditorActionDelegate#refreshAction(org.eclipse.jface.action.IAction)
   */
  @Override
  protected void refreshAction( IAction action )
  {
    // do nothing
  }

}
