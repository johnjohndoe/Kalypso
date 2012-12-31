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
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.io.filefilter.FalseFileFilter;
import org.apache.commons.io.filefilter.IOFileFilter;
import org.apache.commons.io.filefilter.TrueFileFilter;
import org.apache.commons.io.filefilter.WildcardFileFilter;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsomodel1d2d.sim.ISimulation1D2DConstants;
import org.kalypso.kalypsomodel1d2d.sim.NodeResultMinMaxCatcher;
import org.kalypso.kalypsomodel1d2d.sim.ResultManager;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.RemoveThemeCommand;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

public class ResultMeta1d2dHelper
{
  public static final String SHORT_DATE_TIME_FORMAT_RESULT_STEP = "dd.MM.yyyy_HH_mm_z"; //$NON-NLS-1$

  public static final String FULL_DATE_TIME_FORMAT_RESULT_STEP = "dd.MM.yyyy_HH_mm_ss_SSS_z"; //$NON-NLS-1$

  public static final String ORIGINAL_2D_FILE_NAME = "original.2d"; //$NON-NLS-1$

  public static final String SWAN_RAW_DATA_META_NAME = "SWAN-Rohdaten"; //$NON-NLS-1$

  public static final String RMA_RAW_DATA_META_NAME = "RMA-Rohdaten"; //$NON-NLS-1$

  public static final String TIME_STEP_PREFIX = "timestep-"; //$NON-NLS-1$

  public static final String STR_THEME_NAME_ISOAREA = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.11" ); //$NON-NLS-1$

  public static final String STR_THEME_NAME_ISOLINE = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.8" ); //$NON-NLS-1$

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
    final IFolder scenarioFolder = ScenarioHelper.getScenarioFolder();

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
              // FIXME: the result meta needs to be kept as well in this case! -> this decision must be made on the meta level, not the resource level!
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

  // FIXM:E arg!
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
        catch( final Exception e )
        {
          // FIXME: why?! the problem is elsewhere!!
          e.printStackTrace();

          // FIXME:

          final IOFileFilter lNoDirFilter = FalseFileFilter.INSTANCE;

          final WildcardFileFilter lFilter = new WildcardFileFilter( new String[] { "*" } ); //$NON-NLS-1$
          final Collection<File> files = FileUtils.listFiles( resource.getLocation().toFile(), lFilter, lNoDirFilter );
          for( final File lFile : files )
            deleteFileOrDirectory( lFile );

          final IOFileFilter lDirFilter = TrueFileFilter.INSTANCE;
          final Collection<File> dirs = FileUtils.listFiles( resource.getLocation().toFile(), lFilter, lDirFilter );
          for( final File lDir : dirs )
            deleteFileOrDirectory( lDir );
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
                children[i].delete();
              }
            }
            catch( final MalformedURLException e )
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

  private static void deleteFileOrDirectory( final File lDir )
  {
    try
    {
      if( lDir.isDirectory() )
        FileUtils.deleteDirectory( lDir );
      else if( lDir.isFile() )
        Files.delete( lDir.toPath() );
    }
    catch( final IOException e1 )
    {
      e1.printStackTrace();
    }
  }

  /**
   * removes the specified resultMeta file including all of its children
   */
  public static IStatus removeResultMetaFileWithChidren( final IResultMeta resultMeta ) throws CoreException
  {
    final IFeatureBindingCollection<IResultMeta> children = resultMeta.getChildren();

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
      return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.1" ) ); //$NON-NLS-1$

    final IResultMeta parent = resultMeta.getOwner();
    if( parent == null )
      return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.2" ) ); //$NON-NLS-1$

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

  public static IStatus deleteAllByID( final ICalcUnitResultMeta calcUnitMeta, final String[] idsToDelete, final IProgressMonitor monitor )
  {
    return deleteAllByID( calcUnitMeta, idsToDelete, monitor, true );
  }

  public static IStatus deleteAllByID( final ICalcUnitResultMeta calcUnitMeta, final String[] idsToDelete, final IProgressMonitor monitor, final boolean includeOriginal )
  {
    final Set<String> toDelete = new HashSet<>( Arrays.asList( idsToDelete ) );

    final IFeatureBindingCollection<IResultMeta> children = calcUnitMeta.getChildren();

    monitor.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.3" ), children.size() ); //$NON-NLS-1$

    final Set<IStatus> stati = new HashSet<>();

    for( final IResultMeta child : children.toArray( new IResultMeta[children.size()] ) )
    {
      monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.4" ) + child.getName() ); //$NON-NLS-1$

      if( toDelete.contains( child.getId() ) )
      {
        stati.add( removeResult( child, includeOriginal ) );
      }

      ProgressUtilities.worked( monitor, 1 );
    }

    return new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), -1, stati.toArray( new IStatus[stati.size()] ), Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.5" ), null ); //$NON-NLS-1$
  }

  public static IStatus deleteResults( final ICalcUnitResultMeta calcUnitMeta, final Date[] stepsToDelete, final IProgressMonitor monitor )
  {
    final Set<Date> toDelete = new HashSet<>( Arrays.asList( stepsToDelete ) );

    final IFeatureBindingCollection<IResultMeta> children = calcUnitMeta.getChildren();

    monitor.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.3" ), children.size() ); //$NON-NLS-1$

    final Set<IStatus> stati = new HashSet<>();

    for( final IResultMeta child : children.toArray( new IResultMeta[children.size()] ) )
    {
      monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.4" ) + child.getName() ); //$NON-NLS-1$

      if( child instanceof IStepResultMeta )
      {
        final IStepResultMeta stepMeta = (IStepResultMeta)child;
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

  public static IStatus deleteLogAndTins( final ICalcUnitResultMeta calcUnitMeta, final Date[] stepsToDelete, final IProgressMonitor monitor )
  {
    final Set<Date> toDelete = new HashSet<>( Arrays.asList( stepsToDelete ) );

    final IFeatureBindingCollection<IResultMeta> children = calcUnitMeta.getChildren();

    monitor.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.3" ), children.size() ); //$NON-NLS-1$

    final Set<IStatus> stati = new HashSet<>();

    for( final IResultMeta child : children.toArray( new IResultMeta[children.size()] ) )
    {
      monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.4" ) + child.getName() ); //$NON-NLS-1$

      if( child instanceof IStepResultMeta )
      {
        final IStepResultMeta stepMeta = (IStepResultMeta)child;
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
        return toDelete.contains( ISimulation1D2DConstants.STEADY_DATE );

      case maximum:
        return toDelete.contains( ISimulation1D2DConstants.MAXI_DATE );

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
    final IFeatureBindingCollection<IResultMeta> children = calcUnitMeta.getChildren();

    final Set<Date> dates = new HashSet<>();
    for( final IResultMeta child : children )
    {
      if( child instanceof IStepResultMeta )
      {
        final IStepResultMeta stepMeta = (IStepResultMeta)child;
        switch( stepMeta.getStepType() )
        {
          case steady:
            dates.add( ISimulation1D2DConstants.STEADY_DATE );
            break;

          case maximum:
            dates.add( ISimulation1D2DConstants.MAXI_DATE );
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
    final IFeatureBindingCollection<IResultMeta> children = calcUnitMeta.getChildren();

    final HashMap<String, Date> ids = new HashMap<>();
    for( final IResultMeta child : children )
    {
      Date l_date = null;
      if( child instanceof IStepResultMeta )
      {
        final IStepResultMeta stepMeta = (IStepResultMeta)child;
        switch( stepMeta.getStepType() )
        {
          case steady:
            l_date = ISimulation1D2DConstants.STEADY_DATE;
            break;

          case maximum:
            l_date = ISimulation1D2DConstants.MAXI_DATE;
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
        final IDocumentResultMeta document = (IDocumentResultMeta)child;
        if( document.getDocumentType() == IDocumentResultMeta.DOCUMENTTYPE.log )
        {
          // System.out.println( "log to delete: " + document );
          // continue;
        }
      }

      ids.put( child.getId(), l_date );
    }

    return ids;
  }

  public static void deleteResultThemeFromMap( final IStepResultMeta stepResult, final IKalypsoLayerModell modell, final ICommandTarget commandTarget )
  {
    final IKalypsoTheme[] allThemes = modell.getAllThemes();

    final IResultMeta calcUnitMeta = stepResult.getOwner();
    final IFeatureBindingCollection<IResultMeta> children = stepResult.getChildren();

    for( final IResultMeta stepChild : children )
    {
      if( stepChild instanceof IDocumentResultMeta )
      {
        final IDocumentResultMeta docResult = (IDocumentResultMeta)stepChild;

        for( final IKalypsoTheme kalypsoTheme : allThemes )
        {
          if( kalypsoTheme instanceof IKalypsoFeatureTheme )
          {
            final IKalypsoFeatureTheme kft = (IKalypsoFeatureTheme)kalypsoTheme;

            final boolean shouldDelete = isResultTheme( kft, docResult, stepResult, calcUnitMeta );

            if( shouldDelete )
            {
              final RemoveThemeCommand removeThemeCommand = new RemoveThemeCommand( modell, kft );
              commandTarget.postCommand( removeThemeCommand, null );
            }
          }
        }
      }
    }
  }

  private static boolean isResultTheme( final IKalypsoFeatureTheme kft, final IDocumentResultMeta docResult, final IStepResultMeta stepResult, final IResultMeta calcUnitMeta )
  {
    // FIXME: instead add properties to this theme when added (gml-id's of document meta)

    // FIXME: UARGHH. We should implement some other method to recognize the right theme
    final String kftName = kft.getName().getKey().toLowerCase();

    if( kftName == null )
      return false;

    // TODO: why only check for isolines, not isoearea's as well?
    return kftName.contains( docResult.getName().trim().toLowerCase() ) && kftName.contains( calcUnitMeta.getName().trim().toLowerCase() )
        && (kftName.contains( stepResult.getName().trim().toLowerCase() ) || kftName.contains( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper.8" ) ));
  }

  public static Date resolveDateFromResultStep( final FileObject pFileResult )
  {
    try
    {
      return interpreteDateFromURL( pFileResult.getParent().getURL() );
    }
    catch( final FileSystemException e0 )
    {
      // FIXME: will never happen due to gerneric exception caught in the first call
      try
      {
        // DEAD CODE:
        return interpreteRMA10TimeLine( findFirstSpecifiedLine2dFile( pFileResult, "DA" ) ); //$NON-NLS-1$
      }
      catch( final Exception e1 )
      {
      }
    }

    return null;
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
      if( url.toExternalForm().contains( ResultManager.STEADY_PREFIX ) )
      {
        return ResultManager.STEADY_DATE;
      }
      if( url.toExternalForm().contains( ResultManager.MAXI_PREFIX ) )
      {
        return ResultManager.MAXI_DATE;
      }

      final String lStrTimeFormat = SHORT_DATE_TIME_FORMAT_RESULT_STEP;
      final String lStrTimeFormatFull = FULL_DATE_TIME_FORMAT_RESULT_STEP;
      final SimpleDateFormat lSimpleDateFormat = new SimpleDateFormat( lStrTimeFormat );
      final SimpleDateFormat lSimpleDateFormatFull = new SimpleDateFormat( lStrTimeFormatFull );
      final int indexOfStepDate = url.toExternalForm().indexOf( TIME_STEP_PREFIX ) + TIME_STEP_PREFIX.length();

      final String dateString = url.toExternalForm().substring( indexOfStepDate );
      try
      {
        return lSimpleDateFormatFull.parse( dateString );
      }
      catch( final Exception e )
      {
        return lSimpleDateFormat.parse( dateString );
      }
    }
    catch( final Exception e )
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
   */
  public static Date interpreteRMA10TimeLine( final String line )
  {
    if( line.length() < 32 )
      return null;

    final String yearString = line.substring( 6, 13 ).trim();
    final String hourString = line.substring( 18, 32 ).trim();

    final int year = Integer.parseInt( yearString );

    return parseTimelineHour( hourString, year );
  }

  public static Date parseTimelineHour( final String hourString, final int year )
  {
    final BigDecimal hours = NumberUtils.parseQuietDecimal( hourString );
    if( hours == null )
      return null;

    // REMARK: we read the calculation core time with the time zone, as defined in Kalypso Preferences
    final Calendar calendar = Calendar.getInstance( KalypsoCorePlugin.getDefault().getTimeZone() );
    calendar.clear();
    calendar.set( year, 0, 1 );

    BigDecimal wholeHours = hours.setScale( 0, BigDecimal.ROUND_DOWN );
    final BigDecimal wholeMinutes = hours.subtract( wholeHours ).multiply( new BigDecimal( "60" ) ); //$NON-NLS-1$
    if( wholeHours.intValue() > 1 )
    {
      wholeHours = new BigDecimal( wholeHours.intValue() - 1 );
    }
    calendar.add( Calendar.HOUR, wholeHours.intValue() );
    calendar.add( Calendar.MINUTE, wholeMinutes.intValue() );

    final boolean lBoolLeapYear = DateUtilities.isLeapYear( calendar );
    if( lBoolLeapYear && calendar.get( Calendar.DAY_OF_YEAR ) > 59 )
    {
      calendar.clear();
      calendar.set( year, 0, 1 );
      calendar.add( Calendar.HOUR, wholeHours.intValue() - 24 );
      calendar.add( Calendar.MINUTE, wholeMinutes.intValue() );
    }

    return calendar.getTime();
  }

  /**
   * @param file
   *          {@link FileObject} is the file object to search in, with linePrefix {@link String} will be specified what
   *          kind of line should be found, e.g. "DA" - is the time line according to 2d-files documentation
   * @return {@link String} the first matching line
   */
  public static String findFirstSpecifiedLine2dFile( final FileObject file, final String linePrefix ) throws IOException, URISyntaxException
  {
    if( linePrefix == null || linePrefix.equals( "" ) || file == null ) {//$NON-NLS-1$
      return "";//$NON-NLS-1$
    }

    String lStrRes = "";//$NON-NLS-1$
    final String lStrParam = linePrefix.trim().toUpperCase();
    final InputStream lInStream = VFSUtilities.getInputStreamFromFileObject( file );

    final Reader lReader = new InputStreamReader( lInStream );
    final BufferedReader lBufferedReader = new BufferedReader( lReader );
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
    final IFeatureBindingCollection<IResultMeta> children = stepResultMeta.getChildren();
    for( final IResultMeta child : children.toArray( new IResultMeta[children.size()] ) )
    {
      if( child instanceof IStepResultMeta )
      {
        final IStepResultMeta stepMeta = (IStepResultMeta)child;
        if( ResultMeta1d2dHelper.SWAN_RAW_DATA_META_NAME.equalsIgnoreCase( stepMeta.getName() ) )
        {
          return stepMeta.getPath();
        }
      }
      else if( child instanceof IDocumentResultMeta )
      {
        final IDocumentResultMeta stepMeta = (IDocumentResultMeta)child;
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
    final IFeatureBindingCollection<IResultMeta> children = calcUnitMeta.getChildren();
    for( final IResultMeta child : children.toArray( new IResultMeta[children.size()] ) )
    {
      if( child instanceof IStepResultMeta )
      {
        final IStepResultMeta stepMeta = (IStepResultMeta)child;
        final IPath lRes = getSavedSWANRawResultData( stepMeta );
        if( lRes != null )
        {
          return lRes;
        }
      }
    }
    return null;
  }

  /**
   * finds the substring with the name of result type from given {@link String}layer name
   */
  public static String resolveResultTypeFromSldFileName( final String sldFileName, final String strType )
  {
    if( StringUtils.isBlank( strType ) )
      return StringUtils.EMPTY;

    if( StringUtils.isBlank( sldFileName ) )
      return StringUtils.EMPTY;

    final int beginIndex = sldFileName.toLowerCase().indexOf( strType.toLowerCase() ) + strType.length();
    final int endIndex = sldFileName.toLowerCase().indexOf( "style" ); //$NON-NLS-1$
    return sldFileName.substring( beginIndex, endIndex ).toLowerCase();
  }

  /**
   * creates the default style file name for given style type and result document name
   */
  public static String getDefaultStyleFileName( final String styleType, final String resDocumentName )
  {
    return "default" + styleType + resDocumentName + "Style.sld"; //$NON-NLS-1$ //$NON-NLS-2$
  }

  public static boolean containsTerrain( final IStepResultMeta stepResultMeta )
  {
    for( final IResultMeta resultMetaStep : stepResultMeta.getOwner().getChildren() )
    {
      if( resultMetaStep instanceof IDocumentResultMeta )
      {
        if( documentResultContainsTerrain( resultMetaStep ) )
          return true;
      }

      for( final IResultMeta resultMeta : resultMetaStep.getChildren() )
      {
        if( resultMeta instanceof IDocumentResultMeta )
        {
          if( documentResultContainsTerrain( resultMeta ) )
            return true;
        }
      }
    }

    return false;
  }

  public static boolean documentResultContainsTerrain( final IResultMeta resultMeta )
  {
    final IDocumentResultMeta documentResult = (IDocumentResultMeta)resultMeta;
    final DOCUMENTTYPE documentType = documentResult.getDocumentType();

    if( documentType == DOCUMENTTYPE.tinTerrain )
    {
      return true;
    }
    return false;
  }

  /**
   * gets the CalcUnitResultMeta as the papa of all steps
   */
  public static ICalcUnitResultMeta getCalcUnitResultMeta( final IResultMeta result )
  {
    if( result instanceof ICalcUnitResultMeta )
      return (ICalcUnitResultMeta)result;
    else
    {
      final IResultMeta parent = result.getOwner();
      if( parent != null )
      {
        return getCalcUnitResultMeta( parent );
      }
    }
    return null;
  }

  public static IDocumentResultMeta[] findDocumentsByType( final IResultMeta parent, final DOCUMENTTYPE searchType )
  {
    final Collection<IDocumentResultMeta> documents = new ArrayList<>();

    final IFeatureBindingCollection<IResultMeta> children = parent.getChildren();
    for( final IResultMeta child : children )
    {
      if( child instanceof IDocumentResultMeta )
      {
        final IDocumentResultMeta document = (IDocumentResultMeta)child;
        final DOCUMENTTYPE documentType = document.getDocumentType();
        if( searchType == documentType )
          documents.add( document );
      }

      /* Recurse */
      final IDocumentResultMeta[] childDocs = findDocumentsByType( child, searchType );
      documents.addAll( Arrays.asList( childDocs ) );
    }

    return documents.toArray( new IDocumentResultMeta[documents.size()] );
  }
}