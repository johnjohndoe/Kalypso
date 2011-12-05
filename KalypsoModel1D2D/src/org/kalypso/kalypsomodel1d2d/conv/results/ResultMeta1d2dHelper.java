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

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.io.filefilter.FalseFileFilter;
import org.apache.commons.io.filefilter.IOFileFilter;
import org.apache.commons.io.filefilter.TrueFileFilter;
import org.apache.commons.io.filefilter.WildcardFileFilter;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemException;
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
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.StepResultMeta;
import org.kalypso.kalypsomodel1d2d.sim.NodeResultMinMaxCatcher;
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
  public static final String SHORT_DATE_TIME_FORMAT_RESULT_STEP = "dd.MM.yyyy_HH_mm_z"; //$NON-NLS-1$

  public static final String FULL_DATE_TIME_FORMAT_RESULT_STEP = "dd.MM.yyyy_HH_mm_ss_SSS_z"; //$NON-NLS-1$

  public static final String STR_THEME_NAME_SEPARATOR = ", ";

  public static final String ORIGINAL_2D_FILE_NAME = "original.2d"; //$NON-NLS-1$

  public static final String SWAN_RAW_DATA_META_NAME = "SWAN-Rohdaten"; //$NON-NLS-1$

  public static final String RMA_RAW_DATA_META_NAME = "RMA-Rohdaten"; //$NON-NLS-1$

  public static final String TIME_STEP_PREFIX = "timestep-";

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
    return removeResultMetaFile( resultMeta, true );
  }

  /**
   * removes the specified resultMeta file
   */
  private static IStatus removeResultMetaFile( final IResultMeta resultMeta, final boolean removeOriginalRawRes )
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
          {
            if( !removeOriginalRawRes )
            {
              if( resource instanceof IFolder )
              {
                removeResourceFolder( resource, removeOriginalRawRes );
              }
              else if( !resource.getLocation().toOSString().toLowerCase().contains( ORIGINAL_2D_FILE_NAME ) )
              {
                resource.delete( true, new NullProgressMonitor() );
              }
            }
            else
            {
              if( resource instanceof IFolder )
              {
                removeResourceFolder( resource, removeOriginalRawRes );
              }
              else
              {
                resource.delete( true, new NullProgressMonitor() );
              }
              // resource.delete( true, new NullProgressMonitor() );
            }
          }
          else if( resource instanceof IFolder )
          {
            // Also delete non-empty parent folders within that path
            final File[] children = resource.getLocation().toFile().listFiles();
            if( children != null && children.length == 0 )
            {
              resource.delete( true, new NullProgressMonitor() );
            }
          }
        }
        catch( final CoreException e )
        {
          // e.printStackTrace();
          return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.0" ) ); //$NON-NLS-1$
        }
      }
    }

    return Status.OK_STATUS;
  }

  private static void removeResourceFolder( final IResource resource, final boolean removeOriginalRawRes ) throws CoreException
  {
    final File[] children = resource.getLocation().toFile().listFiles();
    if( children != null )
    {
      if( children.length == 0 || removeOriginalRawRes )
      {
        try
        {
          resource.delete( true, new NullProgressMonitor() );
        }
        catch( Exception e )
        {
          IOFileFilter lNoDirFilter = FalseFileFilter.INSTANCE;
          WildcardFileFilter lFilter = new WildcardFileFilter( new String[] { "*" } );
          final Collection< ? > files = FileUtils.listFiles( resource.getLocation().toFile(), lFilter, lNoDirFilter );
          for( final Object lFile : files )
          {
            if( lFile instanceof File )
              FileUtils.deleteQuietly( (File) lFile );
          }

          IOFileFilter lDirFilter = TrueFileFilter.INSTANCE;
          final Collection< ? > dirs = FileUtils.listFiles( resource.getLocation().toFile(), lFilter, lDirFilter );
          for( final Object lDir : dirs )
          {
            if( lDir instanceof File )
              try
              {
                FileUtils.deleteDirectory( (File) lDir );
              }
              catch( IOException e1 )
              {
                e1.printStackTrace();
              }
          }
        }
      }
      else
      {
        for( int i = 0; i < children.length; i++ )
        {
          if( !children[i].getName().toLowerCase().contains( ORIGINAL_2D_FILE_NAME ) )
          { 
            try
            {
              final IResource resourceChild = ResourceUtilities.findFileFromURL( children[i].toURI().toURL() );
              if( resourceChild instanceof IFolder )
              {
                removeResourceFolder( resourceChild, removeOriginalRawRes );
              }
              else
              {
                if( resourceChild != null )
                  resourceChild.delete( true, new NullProgressMonitor() );
                children[ i ].delete();
              }
            }
            catch( MalformedURLException e )
            {
              e.printStackTrace();
            }
          }
        }
      }
    }
    else
    {
      resource.delete( true, new NullProgressMonitor() );
    }

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
    return removeResult( resultMeta, true );
  }

  /**
   * removes the specified result meta entry and all of its children. Files will be removed, too.
   * 
   * @param resultMeta
   *          the result entry
   * @param includeOriginal
   *          defines if the complete result folder including the original.2d.zip file should be deleted if set to
   *          false, the folder including the original result will be kept
   */
  public static IStatus removeResult( final IResultMeta resultMeta, final boolean removeOriginalRawRes )
  {
    if( !removeResultMetaFile( resultMeta, removeOriginalRawRes ).isOK() )
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
  public static IDocumentResultMeta addDocument( final IResultMeta resultMeta, final String name, final String description, final DOCUMENTTYPE type, final IPath path, final IStatus status, final NodeResultMinMaxCatcher minMaxCatcher )
  {
    if( resultMeta == null )
      return null;

    final IDocumentResultMeta document = resultMeta.getChildren().addNew( IDocumentResultMeta.QNAME, IDocumentResultMeta.class );
    document.setName( name );
    document.setDescription( description );
    document.setDocumentType( type );
    document.setPath( path );
    document.setStatus( status );
    if( minMaxCatcher != null )
      document.setMinMaxValues( minMaxCatcher );

    return document;
  }

  public static IStatus deleteAllByID( final ICalcUnitResultMeta calcUnitMeta, final String[] idsToDelete, final IProgressMonitor monitor ) throws CoreException
  {
    return deleteAllByID( calcUnitMeta, idsToDelete, monitor, true );
  }

  public static IStatus deleteAllByID( final ICalcUnitResultMeta calcUnitMeta, final String[] idsToDelete, final IProgressMonitor monitor, final boolean includeOriginal ) throws CoreException
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
        stati.add( removeResult( child, includeOriginal ) );
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
        {
          // System.out.println( "log to delete: " + document );
          // continue;
        }
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
        // final String resultNameNode = getNodeResultLayerName( docResult, stepResult, calcUnitMeta, "*" );
        // tins:
        // final String resultNameIso = getIsolineResultLayerName( docResult, stepResult, calcUnitMeta );
        // final String resultNameIsoOld = getIsolineResultLayerNameOld( docResult, calcUnitMeta );
        // final String resultNameArea = getIsoareaResultLayerName( docResult, stepResult, calcUnitMeta );

        for( final IKalypsoTheme kalypsoTheme : allThemes )
        {
          if( kalypsoTheme instanceof IKalypsoFeatureTheme )
          {
            // UARGHH. We should implement some other method to recognize the right theme
            final IKalypsoFeatureTheme kft = (IKalypsoFeatureTheme) kalypsoTheme;
            final String kftName = kft.getName().getKey().toLowerCase();

            // if( kftName.equals( resultNameNode ) || kftName.equals( resultNameIso ) || kftName.equals(
            // resultNameIsoOld ) || kftName.equals( resultNameArea ) )
            if( kftName != null && kftName.contains( docResult.getName().trim().toLowerCase() ) && kftName.contains( calcUnitMeta.getName().trim().toLowerCase() )
                && (kftName.contains( stepResult.getName().trim().toLowerCase() ) || kftName.contains( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.10" ) )) )
            {
              final RemoveThemeCommand removeThemeCommand = new RemoveThemeCommand( modell, kft );
              commandTarget.postCommand( removeThemeCommand, null );
            }
          }
        }
      }
    }
  }

  public static String getNodeResultLayerName( final IResultMeta docResult, final IResultMeta stepResult, final IResultMeta calcUnitMeta, final String strType )
  {
    return docResult.getName() + STR_THEME_NAME_SEPARATOR + strType + STR_THEME_NAME_SEPARATOR + stepResult.getName() + STR_THEME_NAME_SEPARATOR
        + stepResult.getFeature().getProperty( StepResultMeta.QNAME_PROP_STEP_TYPE ) + STR_THEME_NAME_SEPARATOR + calcUnitMeta.getName();
  }

  public static String getIsolineResultLayerName( IResultMeta docResult, IResultMeta stepResult, IResultMeta calcUnitMeta )
  {
    return docResult.getName()
        + STR_THEME_NAME_SEPARATOR
        + Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.8" ) + STR_THEME_NAME_SEPARATOR + stepResult.getName() + STR_THEME_NAME_SEPARATOR + calcUnitMeta.getName(); //$NON-NLS-1$ //$NON-NLS-2$
  }

  // /**
  // * old theme name
  // *
  // * @deprecated old projects use this theme name.
  // */
  // @Deprecated
  // private static String getIsolineResultLayerNameOld( IResultMeta docResult, IResultMeta calcUnitMeta )
  // {
  //    return docResult.getName() + Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.10" ) + calcUnitMeta.getName(); //$NON-NLS-1$
  // }

  public static String getIsoareaResultLayerName( IResultMeta docResult, IResultMeta stepResult, IResultMeta calcUnitMeta )
  {
    return docResult.getName()
        + STR_THEME_NAME_SEPARATOR
        + Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.11" ) + STR_THEME_NAME_SEPARATOR + stepResult.getName() + STR_THEME_NAME_SEPARATOR + calcUnitMeta.getName(); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   *
   */
  public static Date resolveDateFromResultStep( final FileObject pFileResult )
  {
    Date lDateRes = null;
    try
    {
      lDateRes = interpreteDateFromURL( pFileResult.getParent().getURL() );
    }
    catch( FileSystemException e0 )
    {
      try
      {
        lDateRes = interpreteRMA10TimeLine( findFirstSpecifiedLine2dFile( pFileResult, "DA" ) ); //$NON-NLS-1$
      }
      catch( IOException e1 )
      {
      }
    }
    return lDateRes;
  }

  /**
   * parse the given {@link URL} for the standard(Kalypso-RMA-Results) time step pattern
   * 
   * @return Date from given {@link URL}, if there is no matching pattern return null
   */
  public static Date interpreteDateFromURL( final URL url )
  {
    try
    {
      String lStrTimeFormat = SHORT_DATE_TIME_FORMAT_RESULT_STEP; //$NON-NLS-1$
      String lStrTimeFormatFull = FULL_DATE_TIME_FORMAT_RESULT_STEP; //$NON-NLS-1$
      SimpleDateFormat lSimpleDateFormat = new SimpleDateFormat( lStrTimeFormat );
      SimpleDateFormat lSimpleDateFormatFull = new SimpleDateFormat( lStrTimeFormatFull );
      int indexOfStepDate = url.toExternalForm().indexOf( TIME_STEP_PREFIX ) + TIME_STEP_PREFIX.length();
      // Date lDateRes = lSimpleDateFormat.parse( url.toExternalForm().substring( indexOfStepDate, indexOfStepDate +
      // lStrTimeFormat.length() ) );
      Date lDateRes = null;
      String dateString = url.toExternalForm().substring( indexOfStepDate );
      try
      {
        lDateRes = lSimpleDateFormatFull.parse( dateString );
      }
      catch( Exception e )
      {
        lDateRes = lSimpleDateFormat.parse( dateString );
      }
      return lDateRes;
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    return null;
  }

  /**
   * parse the time string from the "2d" result file with according format, interprets the date given in Kalypso-RMA
   * format, checks the need for additional day in case of leap year
   * 
   * @return {@link Date} interpreted from given line, in case of invalid format or bad string - null
   * 
   */
  public static Date interpreteRMA10TimeLine( final String line )
  {
    if( line.length() >= 32 )
    {
      try
      {
        final String yearString = line.substring( 6, 13 ).trim();
        final String hourString = line.substring( 18, 32 ).trim();

        final int year = Integer.parseInt( yearString );
        final BigDecimal hours = new BigDecimal( hourString );

        // REMARK: we read the calculation core time with the time zone, as defined in Kalypso Preferences
        final Calendar calendar = Calendar.getInstance( KalypsoCorePlugin.getDefault().getTimeZone() );
        calendar.clear();
        calendar.set( year, 0, 1 );

        BigDecimal wholeHours = hours.setScale( 0, BigDecimal.ROUND_DOWN );
        BigDecimal wholeMinutes = hours.subtract( wholeHours ).multiply( new BigDecimal( "60" ) ); //$NON-NLS-1$
        if( wholeHours.intValue() > 1 )
        {
          wholeHours = new BigDecimal( wholeHours.intValue() - 1 );
        }
        calendar.add( Calendar.HOUR, wholeHours.intValue() );
        calendar.add( Calendar.MINUTE, wholeMinutes.intValue() );
        boolean lBoolLeapYear = DateUtilities.isLeapYear( calendar );
        if( lBoolLeapYear && calendar.get( Calendar.DAY_OF_YEAR ) > 59 )
        {
          calendar.clear();
          calendar.set( year, 0, 1 );
          calendar.add( Calendar.HOUR, wholeHours.intValue() - 24 );
          calendar.add( Calendar.MINUTE, wholeMinutes.intValue() );
        }
        return calendar.getTime();
      }
      catch( final NumberFormatException e )
      {
        return null;
      }
    }
    else
    {
      return null;
    }
  }

  /**
   * @param file
   *          {@link FileObject} is the file object to search in, with linePrefix {@link String} will be specified what
   *          kind of line should be found, e.g. "DA" - is the time line according to 2d-files documentation
   * 
   * @return {@link String} the first matching line
   */
  public static String findFirstSpecifiedLine2dFile( final FileObject file, final String linePrefix ) throws IOException
  {
    if( linePrefix == null || linePrefix.equals( "" ) || file == null ) {//$NON-NLS-1$
      return "";//$NON-NLS-1$
    }

    String lStrRes = "";//$NON-NLS-1$
    String lStrParam = linePrefix.trim().toUpperCase();
    InputStream lInStream = FileUtilities.getInputStreamFromFileObject( file );

    Reader lReader = new InputStreamReader( lInStream );
    BufferedReader lBufferedReader = new BufferedReader( lReader );
    try
    {
      while( true )
      {
        lStrRes = lBufferedReader.readLine();
        if( lStrRes != null && lStrRes.length() > 2 && lStrRes.toUpperCase().startsWith( lStrParam ) )
        {
          break;
        }
        else
        {
          if( lStrRes == null )
          {
            lStrRes = "";//$NON-NLS-1$
            break;
          }
        }
      }
    }
    finally
    {
      IOUtils.closeQuietly( lBufferedReader );
      IOUtils.closeQuietly( lReader );
      IOUtils.closeQuietly( lInStream );
    }
    return lStrRes;
  }

  /**
   * @return {@link IPath} to the saved Waves result file, resolved from given {@link IStepResultMeta} result
   */
  public static IPath getSavedSWANRawResultData( final IStepResultMeta stepResultMeta )
  {
    if( ResultMeta1d2dHelper.SWAN_RAW_DATA_META_NAME.equalsIgnoreCase( stepResultMeta.getName() ) )
    {
      return stepResultMeta.getFullPath();
    }
    final IFeatureWrapperCollection<IResultMeta> children = stepResultMeta.getChildren();
    for( final IResultMeta child : children.toArray( new IResultMeta[children.size()] ) )
    {
      if( child instanceof IStepResultMeta )
      {
        final IStepResultMeta stepMeta = (IStepResultMeta) child;
        if( ResultMeta1d2dHelper.SWAN_RAW_DATA_META_NAME.equalsIgnoreCase( stepMeta.getName() ) )
        {
          return stepMeta.getPath();
        }
      }
      else if( child instanceof IDocumentResultMeta )
      {
        final IDocumentResultMeta stepMeta = (IDocumentResultMeta) child;
        if( ResultMeta1d2dHelper.SWAN_RAW_DATA_META_NAME.equalsIgnoreCase( stepMeta.getName() ) )
        {
          return stepMeta.getPath();
        }
      }
    }
    return null;
  }

  /**
   * @return the {@link IPath} to the saved Waves result file, resolved in given {@link ICalcUnitResultMeta}
   */
  public static IPath getSavedSWANRawResultData( final ICalcUnitResultMeta calcUnitMeta )
  {
    final IFeatureWrapperCollection<IResultMeta> children = calcUnitMeta.getChildren();
    for( final IResultMeta child : children.toArray( new IResultMeta[children.size()] ) )
    {
      if( child instanceof IStepResultMeta )
      {
        final IStepResultMeta stepMeta = (IStepResultMeta) child;
        IPath lRes = getSavedSWANRawResultData( stepMeta );
        if( lRes != null )
        {
          return lRes;
        }
      }
    }
    return null;
  }

  /**
   * @return {@link String} the node results layer name based on old name and specified style file and result type
   */
  public static String getNodeResultLayerName( final String oldThemeName, final String sldFileName, final String strType )
  {
    int iCount = 0;
    String lNewName = ""; //$NON-NLS-1$
    StringTokenizer lStrTokenizer = new StringTokenizer( oldThemeName.trim(), STR_THEME_NAME_SEPARATOR.trim() ); //$NON-NLS-1$
    while( lStrTokenizer.hasMoreTokens() )
    {
      if( iCount++ != 1 )
      {
        lNewName += (lStrTokenizer.nextToken()); //$NON-NLS-1$
      }
      else
      {
        lNewName += (resolveResultTypeFromSldFileName( sldFileName, strType ));//$NON-NLS-2$
        lStrTokenizer.nextToken();
      }
      if( lStrTokenizer.hasMoreTokens() )
      {
        lNewName += STR_THEME_NAME_SEPARATOR.trim(); //$NON-NLS-1$
      }
    }
    return lNewName;
  }

  /**
   * finds the substring with the name of result type from given {@link String}layer name
   */
  public static String resolveResultTypeFromSldFileName( final String sldFileName, final String strType )
  {
    if( sldFileName == null || "".equals( sldFileName ) || strType == null || "".equals( strType ) ) { //$NON-NLS-1$ //$NON-NLS-2$
      return ""; //$NON-NLS-1$ 
    }
    int beginIndex = sldFileName.toLowerCase().indexOf( strType.toLowerCase() ) + strType.length();
    int endIndex = sldFileName.toLowerCase().indexOf( "style" ); //$NON-NLS-1$ 
    return sldFileName.substring( beginIndex, endIndex );
  }

  /**
   * creates the default style file name for given style type and result document name
   */
  public static String getDefaultStyleFileName( final String styleType, final String resDocumentName )
  {
    return "default" + styleType + resDocumentName + "Style.sld"; //$NON-NLS-1$ //$NON-NLS-2$
  }

}
