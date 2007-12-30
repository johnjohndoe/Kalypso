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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta.STEPTYPE;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class ResultMeta1d2dHelper
{

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

    final IPath resultPath = resultMeta.getFullPath();

    final IFolder folder = scenarioFolder.getFolder( resultPath );
    if( folder.exists() == true )
    {
      try
      {
        folder.delete( true, new NullProgressMonitor() );
      }
      catch( CoreException e )
      {
        e.printStackTrace();
        return StatusUtilities.statusFromThrowable( e, "Error while deleting obsolete result files." );
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
    for( IResultMeta child : children )
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
   * updates a {@link IScenarioResultMeta} with a specified {@link ICalcUnitResultMeta}.
   */
  public static IStatus updateResultMeta( final IScenarioResultMeta scenarioResultMeta, final ICalcUnitResultMeta newCalcUnitMeta, final boolean isRestart, final boolean isSteadyCalculation, final boolean isUnsteadyCalculation, final Integer restartStep ) throws Exception
  {
    // look for existing results of this calc unit
    // this cannot happen, but just to be sure...
    if( isRestart && isUnsteadyCalculation && restartStep == null )
      return StatusUtilities.createErrorStatus( "Unsteady restart is calculated, but there is no information regarding the restart step. Result database cannot be updated." );

    /* get already existing result meta */
    final ICalcUnitResultMeta oldCalcUnitMeta = scenarioResultMeta.findCalcUnitMetaResult( newCalcUnitMeta.getCalcUnit() );

    if( oldCalcUnitMeta == null )
    {
      /* no old results existing */

      // just copy the new ones in it
      scenarioResultMeta.getChildren().cloneInto( newCalcUnitMeta );

      return Status.OK_STATUS;
    }

    /* old results existing */

    /* insert the new data in it */
    if( isRestart == false )
    {
      /* ************************ no restart ******************************************* */
      /* remove all old result data */

      // first the files
      // check for already overwritten data files.
      IStatus status = overwrittenDataCheck( oldCalcUnitMeta.getChildren(), oldCalcUnitMeta.getChildren(), newCalcUnitMeta.getChildren() );
      if( status != Status.OK_STATUS )
        return status;

      // if that was successful clear the complete calc unit meta entry...
      scenarioResultMeta.getChildren().remove( oldCalcUnitMeta );

      // and add the new one
      scenarioResultMeta.getChildren().cloneInto( newCalcUnitMeta );

      return Status.OK_STATUS;
    }

    /* ************************ restart ******************************************* */
    /* explore the old result data */
    final IFeatureWrapperCollection<IResultMeta> oldChildren = oldCalcUnitMeta.getChildren();
    final List<IResultMeta> oldChildrenToRemove = new ArrayList<IResultMeta>();

    for( final IResultMeta resultMeta : oldChildren )
    {
      if( resultMeta instanceof IStepResultMeta )
      {
        final IStepResultMeta oldStepResultMeta = (IStepResultMeta) resultMeta;

        // delete the already existing steady result meta, if a new one was computed
        if( isSteadyCalculation && oldStepResultMeta.getStepType() == STEPTYPE.steady )
          oldChildrenToRemove.add( oldStepResultMeta );

        // delete the old maximum
        // TODO: maybe it is good to save the old maximum or compare it with the new one in order to get an overall
        // unsteady maximum result
        else if( isUnsteadyCalculation && oldStepResultMeta.getStepType() == STEPTYPE.maximum )
          oldChildrenToRemove.add( oldStepResultMeta );
        else
        {
          final Integer stepNumber = oldStepResultMeta.getStepNumber();
          if( stepNumber != null )
          {
            if( isUnsteadyCalculation && stepNumber >= restartStep )
              oldChildrenToRemove.add( oldStepResultMeta );

            final STEPTYPE stepType = oldStepResultMeta.getStepType();
            if( stepType.equals( STEPTYPE.error ) && stepNumber == -1 )
              oldChildrenToRemove.add( oldStepResultMeta );
          }
        }

        // else if( isSteadyCalculation )
        // oldChildrenToRemove.add( oldStepResultMeta );

      }
      else if( resultMeta instanceof IDocumentResultMeta )
      {
        final IDocumentResultMeta oldDocResultMeta = (IDocumentResultMeta) resultMeta;
        if( oldDocResultMeta.getDocumentType() == DOCUMENTTYPE.tinTerrain )
          oldChildrenToRemove.add( oldDocResultMeta );
      }
    }
    final IFeatureWrapperCollection<IResultMeta> newChildren = newCalcUnitMeta.getChildren();

    final IStatus status = overwrittenDataCheck( oldChildren, oldChildrenToRemove, newChildren );
    if( status != Status.OK_STATUS )
      return status;

    for( final IResultMeta resultMeta : newChildren )
      oldChildren.cloneInto( resultMeta );

    return Status.OK_STATUS;
  }

  private static IStatus overwrittenDataCheck( final IFeatureWrapperCollection<IResultMeta> oldChildren, final List<IResultMeta> oldChildrenToRemove, final IFeatureWrapperCollection<IResultMeta> newChildren ) throws CoreException
  {
    /* check if there are old results that are overwritten by new results */
    // removing cannot be done directly in previous loop because of ConcurrentModificationException on interator
    for( final IResultMeta resultToRemove : oldChildrenToRemove )
    {
      boolean removed = false;

      for( IResultMeta newChild : newChildren )
      {
        // check, if new result entry is on the to-remove-list
        final String newPath = "results/" + newChild.getFullPath().toPortableString();
        final String oldPath = resultToRemove.getFullPath().toPortableString();

        if( newPath.equals( oldPath ) )
        {
          // just delete the result meta entry and leave the file
          oldChildren.remove( resultToRemove );
          removed = true;
          break;
        }
      }
      if( removed == false )
      {
        // delete the file...
        IStatus status = removeResultMetaFileWithChidren( resultToRemove );
        if( status != Status.OK_STATUS )
          return status;

        // ... and the meta entry
        oldChildren.remove( resultToRemove );
      }
    }
    return Status.OK_STATUS;
  }

  private static List<IResultMeta> checkForOverwrittenDataFiles( ICalcUnitResultMeta oldCalcUnitMeta, ICalcUnitResultMeta newCalcUnitMeta )
  {
    IFeatureWrapperCollection<IResultMeta> oldEntries = oldCalcUnitMeta.getChildren();
    IFeatureWrapperCollection<IResultMeta> newEntries = newCalcUnitMeta.getChildren();

    final List<IResultMeta> entriesToRemove = new ArrayList<IResultMeta>();

    return entriesToRemove;

  }

  /**
   * checks the consistency of the data of a specified {@link IResultMeta}.
   */
  public static IStatus checkForConsistency( final IResultMeta resultMeta )
  {
    IFeatureWrapperCollection<IResultMeta> children = resultMeta.getChildren();

    if( children.size() == 0 )
      return validateMetaEntries( resultMeta );
    else
      for( IResultMeta child : children )
      {
        return checkForConsistency( child );
      }
    return Status.OK_STATUS;
  }

  /**
   * checks if the specified file exists.
   */
  private static IStatus validateFile( IResultMeta resultMeta )
  {
    /* get the scenario folder */
    final IFolder scenarioFolder = getScenarioFolder();

    final IPath resultPath = resultMeta.getFullPath();

    final IFolder folder = scenarioFolder.getFolder( resultPath );
    if( folder.exists() == true )
      return Status.OK_STATUS;
    else
      return StatusUtilities.createErrorStatus( "result file does not exist." );
  }

  /**
   * checks if the result meta has well formed meta data.
   */
  private static IStatus validateMetaEntries( IResultMeta resultMeta )
  {
    final StringBuffer errorMessage = new StringBuffer( "" );

    if( resultMeta.getName() == "" )
      return StatusUtilities.createErrorStatus( "No name set for result meta entry." );

    if( resultMeta.getDescription() == "" )
      return StatusUtilities.createErrorStatus( "No description set for result meta entry." );

    if( resultMeta.getGmlID() == "" )
      errorMessage.append( "result has no gml-if: " + resultMeta.getName() );

    if( resultMeta instanceof IScenarioResultMeta )
    {
      // IScenarioResultMeta scenarioResult = (IScenarioResultMeta) resultMeta;
    }

    else if( resultMeta instanceof ICalcUnitResultMeta )
    {
      ICalcUnitResultMeta calcUnitResult = (ICalcUnitResultMeta) resultMeta;

      if( calcUnitResult.getCalcStartTime() == null && calcUnitResult.getChildren().size() > 0 )
        errorMessage.append( "No calculation start time set for calc unit entry: " + calcUnitResult.getName() );
      if( calcUnitResult.getCalcEndTime() == null && calcUnitResult.getChildren().size() > 0 )
        errorMessage.append( "No calculation end time set for calc unit entry: " + calcUnitResult.getName() );
      if( validateFile( calcUnitResult ) != Status.OK_STATUS )
        errorMessage.append( "Corresponding file-structure does not exist for calc unit entry: " + calcUnitResult.getName() );

      if( errorMessage.length() > 0 )
        return StatusUtilities.createErrorStatus( errorMessage.toString() );
      else
        return Status.OK_STATUS;
    }

    else if( resultMeta instanceof IStepResultMeta )
    {
      IStepResultMeta stepResult = (IStepResultMeta) resultMeta;

      STEPTYPE stepType = stepResult.getStepType();
      if( stepType == null )
        errorMessage.append( "Step result has type definition: " + stepResult.getName() );
      else
      {
        final Date stepTime = stepResult.getStepTime();
        final Integer stepNumber = stepResult.getStepNumber();

        switch( stepType )
        {
          case steady:
            if( stepNumber != -1 )
              errorMessage.append( "Steady step result has non-valid step number (" + stepNumber + "): " + stepResult.getName() );

            if( stepTime == null )
              errorMessage.append( "Steady step result has non-valid step time (" + stepTime + "): " + stepResult.getName() );

            break;

          case maximum:
            if( stepNumber != -1 )
              errorMessage.append( "Maximum step result has non-valid step number (" + stepNumber + "): " + stepResult.getName() );

            if( stepTime == null )
              errorMessage.append( "Maximum step result has non-valid step time (" + stepTime + "): " + stepResult.getName() );

            break;

          case error:

            break;

          case qSteady:

            break;

          case unsteady:
            if( stepNumber == -1 )
              errorMessage.append( "Unsteady step result has non-valid step number (" + stepNumber + "): " + stepResult.getName() );

            if( stepTime == null )
              errorMessage.append( "Unsteady step result has non-valid step time (" + stepTime + "): " + stepResult.getName() );

          default:
            errorMessage.append( "non-valid step type (" + stepType + "): " + stepResult.getName() );

            break;
        }
      }
      if( errorMessage.length() > 0 )
        return StatusUtilities.createErrorStatus( errorMessage.toString() );
      else
        return Status.OK_STATUS;
    }

    else if( resultMeta instanceof IDocumentResultMeta )
    {
      IDocumentResultMeta docResult = (IDocumentResultMeta) resultMeta;

      final DOCUMENTTYPE docType = docResult.getDocumentType();

      if( docType == null )
        errorMessage.append( "non-valid doc type for: " + docResult.getName() );

      if( validateFile( docResult ) != Status.OK_STATUS )
        errorMessage.append( "Corresponding file-structure does not exist for document result entry: " + docResult.getName() );

      if( errorMessage.length() > 0 )
        return StatusUtilities.createErrorStatus( errorMessage.toString() );
      else
        return Status.OK_STATUS;
    }

    return Status.OK_STATUS;

  }

  /**
   * removes the specified result meta entry and all of its children. Files will be removed, too.
   * 
   * @param resultMeta
   *            the result entry
   */
  public static IStatus removeResult( final IResultMeta resultMeta )
  {
    if( removeResultMetaFile( resultMeta ) != Status.OK_STATUS )
      return StatusUtilities.createErrorStatus( "removeResult: could not delete files." );

    final IResultMeta parent = resultMeta.getParent();
    if( parent == null )
      return StatusUtilities.createErrorStatus( "removeResult: result has no parent." );

    parent.getChildren().remove( resultMeta );

    return Status.OK_STATUS;
  }

  /**
   * adds a specified {@link IDocumentResultMeta} to the specified {@link IResultMeta}
   * 
   * @param resultMeta
   *            result meta to which the document result is added to
   * @param name
   *            name of the document
   * @param description
   *            description of the document
   * @param type
   *            {@link IDocumentResultMeta.DOCUMENTTYPE} of the document
   * @param path
   *            path to that document
   * @param status
   *            status of the document
   * @param minValue
   *            min value of the document
   * @param maxValue
   *            max value of the document
   * 
   */
  public static IDocumentResultMeta addDocument( final IResultMeta resultMeta, final String name, final String description, final DOCUMENTTYPE type, final IPath path, final IStatus status, final BigDecimal minValue, final BigDecimal maxValue )
  {
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

}
