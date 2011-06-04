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
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.kalypso.model.wspm.core.gml.WspmReach;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.ui.light.WspmGmvViewPart;
import org.kalypso.model.wspm.tuhh.ui.light.WspmMapViewPart;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.PoolGmlWorkspaceProvider;
import org.kalypso.ogc.gml.command.ChangeExtentCommand;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.command.RemoveThemeCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ogc.gml.mapmodel.visitor.ThemeUsedForMaxExtentPredicate;
import org.kalypso.ui.action.AddThemeCommand;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * Encapsulates the data project of a pdb connection.
 * 
 * @author Gernot Belger
 */
public class PdbWspmProject
{
  public static final String PROPERTY_THEME_REACH = "pdbReach"; //$NON-NLS-1$

  static final String WSPM_PROJECT_NAME = "PDBWspmData"; //$NON-NLS-1$

  private final ModellEventListener m_modelListener = new ModellEventListener()
  {
    @Override
    public void onModellChange( final ModellEvent modellEvent )
    {
      handleModelChange( modellEvent );
    }
  };

  private PoolGmlWorkspaceProvider m_provider;

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
      final CommandableWorkspace workspace = m_provider.getWorkspace();
      if( workspace != null )
        workspace.removeModellListener( m_modelListener );

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

  public TuhhWspmProject getWspmProject( )
  {
    if( m_provider == null )
      return null;

    final CommandableWorkspace workspace = m_provider.getWorkspace();
    if( workspace == null )
      return null;

    return (TuhhWspmProject) workspace.getRootFeature();
  }

  public void saveProject( final IProgressMonitor monitor ) throws CoreException
  {
    if( m_provider == null )
      return;

    m_provider.save( monitor );
  }

  public void updateViews( final TuhhReach[] toSelect )
  {
    /* Bring gmv view to top and select changed features */
    final WspmGmvViewPart gmvView = findView( WspmGmvViewPart.ID );
    if( gmvView != null )
    {
      try
      {
        final IWorkbenchPage page = m_window.getActivePage();
        page.showView( WspmGmvViewPart.ID, null, IWorkbenchPage.VIEW_ACTIVATE );
        gmvView.getSite().getSelectionProvider().setSelection( new StructuredSelection( toSelect ) );
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

    final CompositeCommand compositeCommand = new CompositeCommand( "Add reach themes" );

    final TuhhWspmProject project = getWspmProject();
    final WspmWaterBody[] waterBodies = project.getWaterBodies();

    /* Remove obsolete themes */
    final ObsoleteReachThemesVisitor obsoleteReachesVisitor = new ObsoleteReachThemesVisitor( project );
    mapModell.accept( obsoleteReachesVisitor, IKalypsoThemeVisitor.DEPTH_INFINITE );
    final IKalypsoTheme[] obsoleteThemes = obsoleteReachesVisitor.getObsoleteThemes();
    for( final IKalypsoTheme theme : obsoleteThemes )
      compositeCommand.addCommand( new RemoveThemeCommand( mapModell, theme, true ) );

    /* Add necessary themes */
    for( final WspmWaterBody waterBody : waterBodies )
    {
      final WspmReach[] reaches = waterBody.getReaches();
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
    if( modellEvent instanceof FeatureStructureChangeModellEvent )
      updateMap();
  }

  public <T extends IViewPart> T findView( final String id )
  {
    final FindViewRunnable<T> runnable = new FindViewRunnable<T>( id, m_window );
    return runnable.execute();
  }

  public IProject getProject( )
  {
    return m_project;
  }

  void setData( final PoolGmlWorkspaceProvider provider, final IProject project )
  {
    m_provider = provider;
    m_project = project;

    m_provider.getWorkspace().addModellListener( m_modelListener );
  }
}