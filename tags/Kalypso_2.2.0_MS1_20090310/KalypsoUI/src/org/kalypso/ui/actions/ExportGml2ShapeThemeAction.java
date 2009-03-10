package org.kalypso.ui.actions;

import java.io.File;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionDelegate2;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.serialize.Gml2ShapeConverter;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.io.shpapi.ShapeConst;
import org.kalypsodeegree_impl.io.shpapi.dataprovider.IShapeDataProvider;
import org.kalypsodeegree_impl.io.shpapi.dataprovider.TriangulatedSurfaceSinglePartShapeDataProvider;
import org.kalypsodeegree_impl.model.geometry.GM_TriangulatedSurface_Impl;

public class ExportGml2ShapeThemeAction implements IObjectActionDelegate, IActionDelegate2
{
  private IAction m_action;

  private ISelection m_selection;

  private static final String SETTINGS_LAST_DIR = "lastDir"; //$NON-NLS-1$

  /**
   * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IWorkbenchPart)
   */
  public void setActivePart( final IAction action, final IWorkbenchPart targetPart )
  {
    m_action = action;

    updateAction();
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    throw new UnsupportedOperationException( Messages.getString( "org.kalypso.ui.actions.ExportGml2ShapeThemeAction.1" ) ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    m_action = action;
    m_selection = selection;

    updateAction();
  }

  /**
   * @see org.eclipse.ui.IActionDelegate2#dispose()
   */
  public void dispose( )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.eclipse.ui.IActionDelegate2#init(org.eclipse.jface.action.IAction)
   */
  public void init( final IAction action )
  {
    m_action = action;

    updateAction();
  }

  /**
   * @see org.eclipse.ui.IActionDelegate2#runWithEvent(org.eclipse.jface.action.IAction, org.eclipse.swt.widgets.Event)
   */
  public void runWithEvent( final IAction action, final Event event )
  {
    final Shell shell = event.display.getActiveShell();

    final IStructuredSelection sel = (IStructuredSelection) m_selection;
    final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) sel.getFirstElement();
    final FeatureList featureList = theme == null ? null : theme.getFeatureList();
    if( featureList == null || featureList.isEmpty() )
    {
      MessageDialog.openWarning( shell, action.getText(), Messages.getString( "org.kalypso.ui.actions.ExportGml2ShapeThemeAction.2" ) ); //$NON-NLS-1$
      return;
    }

    final Gml2ShapeConverter converter = Gml2ShapeConverter.createDefault( theme.getFeatureType() );

    // examine what we got and ask user
    // TODO: only use file extension which make sense (dbf OR shp)

    /* ask user for file */
    final IDialogSettings dialogSettings = PluginUtilities.getDialogSettings( KalypsoGisPlugin.getDefault(), "gml2shapeExport" ); //$NON-NLS-1$
    final String lastDirPath = dialogSettings.get( SETTINGS_LAST_DIR );
    final FileDialog fileDialog = new FileDialog( shell, SWT.SAVE );
    fileDialog.setFilterExtensions( new String[] { "*.*", "*.shp", "*.dbf" } ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    fileDialog.setFilterNames( new String[] {
        Messages.getString( "org.kalypso.ui.actions.ExportGml2ShapeThemeAction.7" ), Messages.getString( "org.kalypso.ui.actions.ExportGml2ShapeThemeAction.8" ), Messages.getString( "org.kalypso.ui.actions.ExportGml2ShapeThemeAction.9" ) } ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    fileDialog.setText( action.getText() );
    if( lastDirPath != null )
    {
      fileDialog.setFilterPath( lastDirPath );
      fileDialog.setFileName( theme.getLabel() );
    }
    else
    {
      fileDialog.setFileName( theme.getLabel() );
    }
    final String result = fileDialog.open();
    if( result == null )
      return;

    dialogSettings.put( SETTINGS_LAST_DIR, new File( result ).getParent() );

    final String shapeFileBase;
    if( result.toLowerCase().endsWith( ".shp" ) || result.toLowerCase().endsWith( ".dbf" ) )
    {
      shapeFileBase = FileUtilities.nameWithoutExtension( result );
    }
    else
    {
      shapeFileBase = result;
    }

    final Job job = new Job( action.getText() + " - " + result ) //$NON-NLS-1$
    {
      @SuppressWarnings("unchecked")//$NON-NLS-1$
      @Override
      protected IStatus run( final IProgressMonitor monitor )
      {
        IShapeDataProvider shapeDataProvider = null;

        final Feature feature = (Feature) featureList.get( 0 );
        final GM_Object geometryProperty = feature.getDefaultGeometryProperty();
        if( geometryProperty instanceof GM_TriangulatedSurface_Impl )
        {
          final byte shapeType = ShapeConst.SHAPE_TYPE_POLYGONZ;
          shapeDataProvider = new TriangulatedSurfaceSinglePartShapeDataProvider( (Feature[]) featureList.toArray( new Feature[featureList.size()] ), shapeType );
        }

        try
        {
          converter.writeShape( featureList, shapeFileBase, shapeDataProvider, monitor );
        }
        catch( final CoreException e )
        {
          return e.getStatus();
        }
        return Status.OK_STATUS;
      }
    };
    job.setUser( true );
    job.schedule();
  }

  private void updateAction( )
  {
    // here we could disable the action if no data is available, listening to the theme would be necessary then
    if( m_action != null )
    {
      m_action.setEnabled( true );
    }
  }

}
