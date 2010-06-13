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
package org.kalypso.kalypsomodel1d2d.conv.results;

import java.io.File;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsomodel1d2d.sim.ResultManager;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.RemoveThemeCommand;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class ResultMeta1d2dHelper
{
  // TODO: remove!
  private static IFolder getScenarioFolder( )
  {
    /* get the scenario folder */
    final IHandlerService handlerService = (IHandlerService) PlatformUI.getWorkbench().getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    return (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
  }

  /**
   * removes the specified resultMeta file
   */
  private static IStatus removeResultMetaFile( final IResultMeta resultMeta )
  {
    /* get the scenario folder */
    final IFolder scenarioFolder = getScenarioFolder();

    final IPath fullResultPath = resultMeta.getFullPath();
    final IPath resultPath = resultMeta.getPath();

    for( int i = 0; i < resultPath.segmentCount(); i++ )
    {
      final IPath currentPath = fullResultPath.uptoSegment( fullResultPath.segmentCount() - i );

      // REMARK: we cannot assume that the child is a sub-folder and hence cannot use folder.exist here
      final IResource resource = scenarioFolder.findMember( currentPath );
      if( resource != null )
      {
        try
        {
          if( i == 0 )
            resource.delete( true, new NullProgressMonitor() );
          else if( resource instanceof IFolder )
          {
            // Also delete non-empty parent folders within that path
            final File[] children = resource.getLocation().toFile().listFiles();
            if( children != null && children.length == 0 )
              resource.delete( true, new NullProgressMonitor() );
          }
        }
        catch( final CoreException e )
        {
          e.printStackTrace();
          return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.0" ) ); //$NON-NLS-1$
        }
      }
    }

    return Status.OK_STATUS;
  }

  /**
   * removes the specified resultMeta file including all of its children
   */
  public static IStatus removeResultMetaFileWithChidren( final IResultMeta resultMeta ) throws CoreException
  {
    final IFeatureWrapperCollection<IResultMeta> children = resultMeta.getChildren();

    /* delete children */
    for( final IResultMeta child : children )
    {
      final IStatus status = removeResultMetaFileWithChidren( child );
      if( status != Status.OK_STATUS )
        return status;
    }

    /* delete parent */
    final IStatus status = removeResultMetaFile( resultMeta );
    if( status != Status.OK_STATUS )
      return status;
    return Status.OK_STATUS;

  }

  /**
   * removes the specified result meta entry and all of its children. Files will be removed, too.
   * 
   * @param resultMeta
   *          the result entry
   */
  public static IStatus removeResult( final IResultMeta resultMeta )
  {
    if( !removeResultMetaFile( resultMeta ).isOK() )
      return StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.1" ) ); //$NON-NLS-1$

    final IResultMeta parent = resultMeta.getParent();
    if( parent == null )
      return StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.2" ) ); //$NON-NLS-1$

    parent.getChildren().remove( resultMeta );

    return Status.OK_STATUS;
  }

  /**
   * adds a specified {@link IDocumentResultMeta} to the specified {@link IResultMeta}
   * 
   * @param resultMeta
   *          result meta to which the document result is added to
   * @param name
   *          name of the document
   * @param description
   *          description of the document
   * @param type
   *          {@link IDocumentResultMeta.DOCUMENTTYPE} of the document
   * @param path
   *          path to that document
   * @param status
   *          status of the document
   * @param minValue
   *          min value of the document
   * @param maxValue
   *          max value of the document
   * 
   */
  public static IDocumentResultMeta addDocument( final IResultMeta resultMeta, final String name, final String description, final DOCUMENTTYPE type, final IPath path, final IStatus status, final BigDecimal minValue, final BigDecimal maxValue )
  {
    if( resultMeta == null )
      return null;

    final IDocumentResultMeta document = resultMeta.getChildren().addNew( IDocumentResultMeta.QNAME, IDocumentResultMeta.class );
    document.setName( name );
    document.setDescription( description );
    document.setDocumentType( type );
    document.setPath( path );
    document.setStatus( status );
    if( minValue != null )
      document.setMinValue( minValue );
    if( maxValue != null )
      document.setMaxValue( maxValue );

    return document;
  }

  public static IStatus deleteAllByID( final ICalcUnitResultMeta calcUnitMeta, final String[] idsToDelete, final IProgressMonitor monitor ) throws CoreException
  {
    final Set<String> toDelete = new HashSet<String>( Arrays.asList( idsToDelete ) );

    final IFeatureWrapperCollection<IResultMeta> children = calcUnitMeta.getChildren();

    monitor.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.3" ), children.size() ); //$NON-NLS-1$

    final Set<IStatus> stati = new HashSet<IStatus>();

    for( final IResultMeta child : children.toArray( new IResultMeta[children.size()] ) )
    {
      monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.4" ) + child.getName() ); //$NON-NLS-1$

      if( toDelete.contains( child.getGmlID() ) )
      {
        stati.add( removeResult( child ) );
      }

      ProgressUtilities.worked( monitor, 1 );
    }

    return new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), -1, stati.toArray( new IStatus[stati.size()] ), Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.5" ), null ); //$NON-NLS-1$
  }

  public static IStatus deleteResults( final ICalcUnitResultMeta calcUnitMeta, final Date[] stepsToDelete, final IProgressMonitor monitor ) throws CoreException
  {
    final Set<Date> toDelete = new HashSet<Date>( Arrays.asList( stepsToDelete ) );

    final IFeatureWrapperCollection<IResultMeta> children = calcUnitMeta.getChildren();

    monitor.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.3" ), children.size() ); //$NON-NLS-1$

    final Set<IStatus> stati = new HashSet<IStatus>();

    for( final IResultMeta child : children.toArray( new IResultMeta[children.size()] ) )
    {
      monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.4" ) + child.getName() ); //$NON-NLS-1$

      if( child instanceof IStepResultMeta )
      {
        final IStepResultMeta stepMeta = (IStepResultMeta) child;
        final boolean delete = checkDeleteResult( toDelete, stepMeta );
        if( delete )
          stati.add( removeResult( stepMeta ) );
      }
      else
      {
        // REMARK: at the moment, we also delete all other child, normally this is the model-tin and the simulation log,
        // which both are recreated during each calculation
        stati.add( removeResult( child ) );
      }

      ProgressUtilities.worked( monitor, 1 );
    }

    return new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), -1, stati.toArray( new IStatus[stati.size()] ), Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.5" ), null ); //$NON-NLS-1$
  }

  public static IStatus deleteLogAndTins( final ICalcUnitResultMeta calcUnitMeta, final Date[] stepsToDelete, final IProgressMonitor monitor ) throws CoreException
  {
    final Set<Date> toDelete = new HashSet<Date>( Arrays.asList( stepsToDelete ) );

    final IFeatureWrapperCollection<IResultMeta> children = calcUnitMeta.getChildren();

    monitor.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.3" ), children.size() ); //$NON-NLS-1$

    final Set<IStatus> stati = new HashSet<IStatus>();

    for( final IResultMeta child : children.toArray( new IResultMeta[children.size()] ) )
    {
      monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.4" ) + child.getName() ); //$NON-NLS-1$

      if( child instanceof IStepResultMeta )
      {
        final IStepResultMeta stepMeta = (IStepResultMeta) child;
        final boolean delete = checkDeleteResult( toDelete, stepMeta );
        if( delete )
          stati.add( removeResult( stepMeta ) );
      }
      else
      {
        // REMARK: at the moment, we also delete all other child, normally this is the model-tin and the simulation log,
        // which both are recreated during each calculation
        stati.add( removeResult( child ) );
      }

      ProgressUtilities.worked( monitor, 1 );
    }

    return new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), -1, stati.toArray( new IStatus[stati.size()] ), Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.5" ), null ); //$NON-NLS-1$
  }

  private static boolean checkDeleteResult( final Set<Date> toDelete, final IStepResultMeta stepMeta )
  {
    switch( stepMeta.getStepType() )
    {
      case steady:
        return toDelete.contains( ResultManager.STEADY_DATE );

      case maximum:
        return toDelete.contains( ResultManager.MAXI_DATE );

      case unsteady:
        final Date stepTime = stepMeta.getStepTime();
        return toDelete.contains( stepTime );

      default:
        return false;
    }
  }

  /**
   * Collects the dates of all steps of the given calc-unit-result.<br>
   * Steady and Maximum steps get the specific Date constants from ResultManager.
   */
  public static Date[] getStepDates( final ICalcUnitResultMeta calcUnitMeta )
  {
    final IFeatureWrapperCollection<IResultMeta> children = calcUnitMeta.getChildren();

    final Set<Date> dates = new HashSet<Date>();
    for( final IResultMeta child : children )
    {
      if( child instanceof IStepResultMeta )
      {
        final IStepResultMeta stepMeta = (IStepResultMeta) child;
        switch( stepMeta.getStepType() )
        {
          case steady:
            dates.add( ResultManager.STEADY_DATE );
            break;

          case maximum:
            dates.add( ResultManager.MAXI_DATE );
            break;
          case unsteady:
            final Date stepTime = stepMeta.getStepTime();
            dates.add( stepTime );
            break;

          default:
            break;
        }
      }
    }

    return dates.toArray( new Date[dates.size()] );
  }

  public static Map<String, Date> getAllIDs( final ICalcUnitResultMeta calcUnitMeta )
  {
    final IFeatureWrapperCollection<IResultMeta> children = calcUnitMeta.getChildren();

    final HashMap<String, Date> ids = new HashMap<String, Date>();
    for( final IResultMeta child : children )
    {
      Date l_date = null;
      if( child instanceof IStepResultMeta )
      {
        final IStepResultMeta stepMeta = (IStepResultMeta) child;
        switch( stepMeta.getStepType() )
        {
          case steady:
            l_date = ResultManager.STEADY_DATE;
            break;

          case maximum:
            l_date = ResultManager.MAXI_DATE;
            break;
          case unsteady:
            final Date stepTime = stepMeta.getStepTime();
            l_date = stepTime;
            break;

          default:
            break;
        }
      }
      // log file should not be included in the list
      else if( child instanceof IDocumentResultMeta )
      {
        final IDocumentResultMeta document = (IDocumentResultMeta) child;
        if( document.getDocumentType() == IDocumentResultMeta.DOCUMENTTYPE.log )
          continue;
      }

      ids.put( child.getGmlID(), l_date );
    }

    return ids;
  }

  public static void deleteResultThemeFromMap( final IStepResultMeta stepResult, final IKalypsoLayerModell modell, final ICommandTarget commandTarget )
  {
    final IKalypsoTheme[] allThemes = modell.getAllThemes();

    final IResultMeta calcUnitMeta = stepResult.getParent();
    final IFeatureWrapperCollection<IResultMeta> children = stepResult.getChildren();

    for( final IResultMeta stepChild : children )
    {
      if( stepChild instanceof IDocumentResultMeta )
      {
        final IDocumentResultMeta docResult = (IDocumentResultMeta) stepChild;

        // nodes:
        final String resultNameNode = getNodeResultLayerName( docResult, stepResult, calcUnitMeta );

        // tins:
        final String resultNameIso = getIsolineResultLayerName( docResult, stepResult, calcUnitMeta );
        final String resultNameIsoOld = getIsolineResultLayerNameOld( docResult, calcUnitMeta );
        final String resultNameArea = getIsoareaResultLayerName( docResult, stepResult, calcUnitMeta );

        for( final IKalypsoTheme kalypsoTheme : allThemes )
        {
          if( kalypsoTheme instanceof IKalypsoFeatureTheme )
          {
            // UARGHH. We should implement some other method to recognize the right theme
            final IKalypsoFeatureTheme kft = (IKalypsoFeatureTheme) kalypsoTheme;
            final String kftName = kft.getName().getKey();

            if( kftName.equals( resultNameNode ) || kftName.equals( resultNameIso ) || kftName.equals( resultNameIsoOld ) || kftName.equals( resultNameArea ) )
            {
              final RemoveThemeCommand removeThemeCommand = new RemoveThemeCommand( modell, kft );
              commandTarget.postCommand( removeThemeCommand, null );
            }
          }
        }
      }
    }
  }

  public static String getNodeResultLayerName( IResultMeta docResult, IResultMeta stepResult, IResultMeta calcUnitMeta )
  {
    return docResult.getName() + ", " + stepResult.getName() + ", " + calcUnitMeta.getName(); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public static String getIsolineResultLayerName( IResultMeta docResult, IResultMeta stepResult, IResultMeta calcUnitMeta )
  {
    return docResult.getName() + Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.8" ) + stepResult.getName() + ", " + calcUnitMeta.getName(); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * old theme name
   * 
   * @deprecated old projects use this theme name.
   */
  @Deprecated
  private static String getIsolineResultLayerNameOld( IResultMeta docResult, IResultMeta calcUnitMeta )
  {
    return docResult.getName() + Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.10" ) + calcUnitMeta.getName(); //$NON-NLS-1$
  }

  public static String getIsoareaResultLayerName( IResultMeta docResult, IResultMeta stepResult, IResultMeta calcUnitMeta )
  {
    return docResult.getName() + Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.11" ) + stepResult.getName() + ", " + calcUnitMeta.getName(); //$NON-NLS-1$ //$NON-NLS-2$
  }

}
