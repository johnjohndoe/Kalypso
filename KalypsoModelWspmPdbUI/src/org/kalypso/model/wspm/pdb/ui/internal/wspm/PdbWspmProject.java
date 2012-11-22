/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.pdb.ui.internal.wspm;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.progress.IProgressService;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.core.gml.WspmReach;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.IPdbWspmProject;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.ui.light.WspmGmvViewPart;
import org.kalypso.model.wspm.tuhh.ui.light.WspmMapViewPart;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IFeaturesProvider;
import org.kalypso.ogc.gml.IFeaturesProviderListener;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.PoolFeaturesProvider;
import org.kalypso.ogc.gml.command.ChangeExtentCommand;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.command.RemoveThemeCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ogc.gml.mapmodel.visitor.ThemeUsedForMaxExtentPredicate;
import org.kalypso.ui.action.AddThemeCommand;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * Encapsulates the data project of a pdb connection.
 * 
 * @author Gernot Belger
 */
public class PdbWspmProject implements IPdbWspmProject
{
  public static final String PROPERTY_THEME_REACH = "pdbReach"; //$NON-NLS-1$

  static final String WSPM_PROJECT_NAME = "PDBWspmData"; //$NON-NLS-1$

  static final String STR_SAVE_LOCAL_DATA_TITLE = Messages.getString( "PdbWspmProject.0" ); //$NON-NLS-1$

  private final IFeaturesProviderListener m_modelListener = new IFeaturesProviderListener()
  {
    @Override
    public void featuresChanged( final IFeaturesProvider source, final ModellEvent modellEvent )
    {
      handleModelChange( modellEvent );
    }
  };

  private PoolFeaturesProvider m_provider;

  private IProject m_project;

  private final IWorkbenchWindow m_window;

  public PdbWspmProject( final IWorkbenchWindow window )
  {
    m_window = window;
  }

  public void dispose( )
  {
    if( m_provider != null )
    {
      m_provider.removeFeaturesProviderListener( m_modelListener );
      m_provider.dispose();
    }
    m_provider = null;
  }

  public void loadData( final IProgressMonitor monitor ) throws CoreException
  {
    Assert.isTrue( m_provider == null );

    final LoadPdbDataOperation operation = new LoadPdbDataOperation( this );
    final IStatus result = operation.execute( monitor );
    if( !result.isOK() )
      throw new CoreException( result );
  }

  IFile getModelFile( )
  {
    return m_project.getFile( IWspmTuhhConstants.FILE_MODELL_GML );
  }

  IFile getMapFile( )
  {
    return m_project.getFile( "PDB.gmt" ); //$NON-NLS-1$
  }

  @Override
  public TuhhWspmProject getWspmProject( )
  {
    if( m_provider == null )
      return null;

    final CommandableWorkspace workspace = m_provider.getWorkspace();
    if( workspace == null )
      return null;

    return (TuhhWspmProject)workspace.getRootFeature();
  }

  @Override
  public void doSave( final IProgressMonitor monitor ) throws CoreException
  {
    if( m_provider == null )
      return;

    m_provider.save( monitor );
  }

  public void updateViews( final Object[] toSelect )
  {
    /* Bring gmv view to top and select changed features */
    final WspmGmvViewPart gmvView = findView( WspmGmvViewPart.ID );
    if( gmvView != null )
    {
      try
      {
        final IWorkbenchPage page = m_window.getActivePage();
        page.showView( WspmGmvViewPart.ID, null, IWorkbenchPage.VIEW_ACTIVATE );
        if( toSelect != null )
        {
          final TreeViewer treeViewer = gmvView.getTreeView().getTreeViewer();
          treeViewer.setSelection( new StructuredSelection( toSelect ) );
          treeViewer.setExpandedElements( toSelect );
        }
      }
      catch( final PartInitException e )
      {
        e.printStackTrace();
      }
    }

    /* Jump to freshly downloaded items */
    final WspmMapViewPart mapView = findView( WspmMapViewPart.ID );
    if( mapView == null )
      return;

    final GisTemplateMapModell mapModell = mapView.getMapModell();
    final FindReachThemesVisitor findReachesVisitor = new FindReachThemesVisitor();
    mapModell.accept( findReachesVisitor, IKalypsoThemeVisitor.DEPTH_INFINITE );
    final IKalypsoFeatureTheme[] themes = findReachesVisitor.getThemes( toSelect );
    final GM_Envelope fullExtent = MapModellHelper.calculateExtent( themes, new ThemeUsedForMaxExtentPredicate() );
    final ChangeExtentCommand command = new ChangeExtentCommand( mapView.getMapPanel(), fullExtent );
    mapView.postCommand( command, null );
  }

  @Override
  public CommandableWorkspace getWorkspace( )
  {
    if( m_provider == null )
      return null;

    return m_provider.getWorkspace();
  }

  /* Make sure, that all reaches of the project have a theme in the current map */
  void updateMap( )
  {
    final WspmMapViewPart mapView = findView( WspmMapViewPart.ID );
    if( mapView == null )
      return;

    final GisTemplateMapModell mapModell = mapView.getMapModell();
    if( mapModell == null )
      return;

    final FindReachThemesVisitor findReachesVisitor = new FindReachThemesVisitor();
    mapModell.accept( findReachesVisitor, IKalypsoThemeVisitor.DEPTH_INFINITE );

    final CompositeCommand compositeCommand = new CompositeCommand( Messages.getString( "PdbWspmProject.1" ) ); //$NON-NLS-1$

    final TuhhWspmProject project = getWspmProject();
    final IFeatureBindingCollection<WspmWaterBody> waterBodies = project.getWaterBodies();

    /* Remove obsolete themes */
    final ObsoleteReachThemesVisitor obsoleteReachesVisitor = new ObsoleteReachThemesVisitor( project );
    mapModell.accept( obsoleteReachesVisitor, IKalypsoThemeVisitor.DEPTH_INFINITE );
    final IKalypsoTheme[] obsoleteThemes = obsoleteReachesVisitor.getObsoleteThemes();
    for( final IKalypsoTheme theme : obsoleteThemes )
      compositeCommand.addCommand( new RemoveThemeCommand( mapModell, theme, true ) );

    /* Add necessary themes */
    for( final WspmWaterBody waterBody : waterBodies )
    {
      final IFeatureBindingCollection<WspmReach> reaches = waterBody.getReaches();
      for( final WspmReach reach : reaches )
      {
        final String reachGmlID = reach.getId();
        if( !findReachesVisitor.hasReachTheme( reachGmlID ) )
        {
          final AddThemeCommand newTheme = addReachTheme( mapModell, reach );
          if( newTheme != null )
            compositeCommand.addCommand( newTheme );
        }
      }
    }

    if( compositeCommand.getCommands().length == 0 )
      return;

    mapView.postCommand( compositeCommand, null );

    mapView.doSave( new NullProgressMonitor() );
  }

  private AddThemeCommand addReachTheme( final GisTemplateMapModell mapModell, final WspmReach reach )
  {
    final String name = reach.getName();
    final String type = "gml"; //$NON-NLS-1$

    final String featurePath = String.format( "#fid#%s/%s", reach.getId(), TuhhReach.QNAME_MEMBER_REACHSEGMENT.getLocalPart() ); //$NON-NLS-1$

    final String source = IWspmTuhhConstants.FILE_MODELL_GML;
    final AddThemeCommand command = new AddThemeCommand( mapModell, name, type, featurePath, source );
    command.setShouldActivateTheme( false );

    command.addProperty( PROPERTY_THEME_REACH, reach.getId() );
    command.addProperty( IKalypsoTheme.PROPERTY_DELETEABLE, Boolean.FALSE.toString() );

    return command;
  }

  protected void handleModelChange( final ModellEvent modellEvent )
  {
    if( modellEvent == null || modellEvent instanceof FeatureStructureChangeModellEvent )
      updateMap();
  }

  <T extends IViewPart> T findView( final String id )
  {
    final FindViewRunnable<T> runnable = new FindViewRunnable<>( id, m_window, true );
    return runnable.execute();
  }

  @Override
  public IProject getProject( )
  {
    return m_project;
  }

  void setData( final PoolFeaturesProvider provider, final IProject project )
  {
    m_provider = provider;
    m_project = project;

    m_provider.addFeaturesProviderListener( m_modelListener );
  }

  /**
   * Checks if the data should be save, and asks the user what to do.<br/>
   * Show a 'Yes', 'No', 'Cancel' dialog.
   * 
   * @param If
   *          set to <code>true</code>, the data will be reloaded if the user chooses 'NO'.
   * @return <code>false</code>, if the user cancels the operation.
   */
  public boolean askForProjectSave( )
  {
    if( m_provider == null )
      return true;

    final boolean dirty = m_provider.isDirty();
    if( !dirty )
      return true;

    final Shell shell = m_window.getShell();
    final String message = Messages.getString( "PdbWspmProject.2" ); //$NON-NLS-1$
    final String[] buttonLabels = new String[] { IDialogConstants.YES_LABEL, IDialogConstants.NO_LABEL, IDialogConstants.CANCEL_LABEL };
    final MessageDialog dialog = new MessageDialog( shell, STR_SAVE_LOCAL_DATA_TITLE, null, message, MessageDialog.QUESTION_WITH_CANCEL, buttonLabels, 0 );
    final int result = dialog.open();

    if( result == 0 )
    {
      final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
      {
        @Override
        public IStatus execute( final IProgressMonitor monitor ) throws CoreException
        {
          doSave( monitor );
          return Status.OK_STATUS;
        }
      };

      return busyCursorWhile( operation, STR_SAVE_LOCAL_DATA_TITLE, Messages.getString( "PdbWspmProject.3" ) ); //$NON-NLS-1$
    }
    else if( result == 1 )
      return true;
    else if( result == 2 || result == -1 )
      return false;

    throw new IllegalStateException();
  }

  /**
   * Checks if the data should be save, and asks the user what to do.<br/>
   * Shows a 'OK', 'Cancel' dialog.
   * 
   * @return <code>false</code>, if the user cancels the operation.
   */
  public boolean confirmProjectSave( )
  {
    if( m_provider == null )
      return true;

    final boolean dirty = m_provider.isDirty();
    if( !dirty )
      return true;

    final Shell shell = m_window.getShell();
    final String message = Messages.getString( "PdbWspmProject.4" ); //$NON-NLS-1$
    final boolean result = MessageDialog.openConfirm( shell, STR_SAVE_LOCAL_DATA_TITLE, message );
    if( !result )
      return false;

    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      @Override
      public IStatus execute( final IProgressMonitor monitor ) throws CoreException
      {
        doSave( monitor );
        return Status.OK_STATUS;
      }
    };

    return busyCursorWhile( operation, STR_SAVE_LOCAL_DATA_TITLE, Messages.getString( "PdbWspmProject.5" ) ); //$NON-NLS-1$
  }

  private boolean busyCursorWhile( final ICoreRunnableWithProgress operation, final String title, final String errorMessage )
  {
    final IProgressService progressService = (IProgressService)m_window.getService( IProgressService.class );
    final IStatus status = ProgressUtilities.busyCursorWhile( progressService, operation, errorMessage );
    if( status.isOK() )
      return true;

    final Shell shell = m_window.getShell();
    new StatusDialog( shell, status, title ).open();
    /* Do not close workbench on error */
    return false;
  }
}