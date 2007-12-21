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
package org.kalypso.kalypsomodel1d2d.schema.binding.result;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dFileHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta.STEPTYPE;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.ResultMeta;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * @author Thomas Jung
 * 
 */
public class ScenarioResultMeta extends ResultMeta implements IScenarioResultMeta
{
  private static final String ERGEBNISSE = "Ergebnisse";

  private static final String RESULTS = "results";

  public ScenarioResultMeta( final Feature featureToBind )
  {
    super( featureToBind, IScenarioResultMeta.QNAME );

    // set the path and the name of the scenarioResultMeta
    // TODO: better place for this: while creation of a new scenario!!
    // right now, it is set to the following values:

    setPath( new Path( RESULTS ) );
    setName( ERGEBNISSE );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta#findCalcUnitMetaResult(java.lang.String)
   */
  public ICalcUnitResultMeta findCalcUnitMetaResult( final String calcUnitGmlID )
  {
    final IFeatureWrapperCollection<IResultMeta> children = getChildren();
    for( final IResultMeta resultMeta : children )
    {
      if( resultMeta instanceof ICalcUnitResultMeta )
      {
        final ICalcUnitResultMeta calcUnitMeta = (ICalcUnitResultMeta) resultMeta;
        if( calcUnitMeta.getCalcUnit().equals( calcUnitGmlID ) )
          return calcUnitMeta;
      }
    }
    return null;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta#updateResultMeta(org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta,
   *      boolean, boolean, boolean, java.lang.Integer)
   */
  public void updateResultMeta( final ICalcUnitResultMeta newCalcUnitMeta, final boolean isRestart, final boolean isSteadyCalculation, final boolean isUnsteadyCalculation, final Integer restartStep ) throws Exception
  {
    // look for existing results of this calc unit
    // this cannot happen, but just to be sure...
    if( isRestart && isUnsteadyCalculation && restartStep == null )
      throw new RuntimeException( "Unsteady restart is calculated, but there is no information regarding the restart step. Result database cannot be updated." );

    /* get already existing result meta */
    final ICalcUnitResultMeta oldCalcUnitMeta = findCalcUnitMetaResult( newCalcUnitMeta.getCalcUnit() );

    if( oldCalcUnitMeta != null )
    {
      /* insert the new data */
      if( isRestart )
      {

        /* explore the old result metas */
        final IFeatureWrapperCollection<IResultMeta> oldChildren = oldCalcUnitMeta.getChildren();
        final List<IResultMeta> oldChildrenToRemove = new ArrayList<IResultMeta>();
        for( final IResultMeta resultMeta : oldChildren )
        {
          if( resultMeta instanceof IStepResultMeta )
          {
            final IStepResultMeta oldStepResultMeta = (IStepResultMeta) resultMeta;

            // delete the already existing steady result, if a new one is computed
            if( isSteadyCalculation && oldStepResultMeta.getStepType() == STEPTYPE.steady )
              oldChildrenToRemove.add( oldStepResultMeta );

            // delete every already existing step-result which is after the restart step
            else if( isUnsteadyCalculation && oldStepResultMeta.getStepNumber() >= restartStep )
              oldChildrenToRemove.add( oldStepResultMeta );

            // delete the old maximum
            // TODO: maybe it is good to save the old maximum or compare it with the new one in order to get an overall
            // unsteady maximum result
            else if( isUnsteadyCalculation && oldStepResultMeta.getStepType() == STEPTYPE.maximum )
              oldChildrenToRemove.add( oldStepResultMeta );
          }
          else if( resultMeta instanceof IDocumentResultMeta )
          {
            final IDocumentResultMeta oldDocResultMeta = (IDocumentResultMeta) resultMeta;
            if( oldDocResultMeta.getDocumentType() == DOCUMENTTYPE.tinTerrain )
              oldChildrenToRemove.add( oldDocResultMeta );
          }
        }
        final IFeatureWrapperCollection<IResultMeta> newChildren = newCalcUnitMeta.getChildren();

        /* check if there are old results that are overwritten by new results */
        // removing cannot be done directly in previous loop because of ConcurrentModificationException on interator
        for( final IResultMeta resultToRemove : oldChildrenToRemove )
        {
          // check if the new results are copied before or after that code in order to avoid deleting the new
          // ones. --> Done. There are copied before this method, so everything would be deleted directly :-)

          boolean removed = false;
          // Therefore a check:
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
            // delete the file and the meta entry
            if( ResultMeta1d2dFileHelper.removeResultMetaFileWithChidren( resultToRemove ) == Status.OK_STATUS )
              oldChildren.remove( resultToRemove );
          }
        }

        for( final IResultMeta resultMeta : newChildren )
          oldChildren.cloneInto( resultMeta );
      }
      else
      {
        // no restart
        final IStatus status = ResultMeta1d2dFileHelper.removeResultMetaFileWithChidren( oldCalcUnitMeta );
        if( status == Status.OK_STATUS )
          getChildren().remove( oldCalcUnitMeta );

        getChildren().cloneInto( newCalcUnitMeta );
      }
    }
    else
      getChildren().cloneInto( newCalcUnitMeta );
  }

  /**
   * TODO. that interface methods does not belong here; (its also in every other ResultMeta implementation and is also
   * the same): so put it into common helper class
   * 
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta#addDocument(java.lang.String,
   *      java.lang.String, org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE,
   *      java.lang.String)
   */
  public void addDocument( final String name, final String description, final DOCUMENTTYPE type, final IPath path, final IStatus status, final BigDecimal minValue, final BigDecimal maxValue )
  {
    final IDocumentResultMeta document = getChildren().addNew( IDocumentResultMeta.QNAME, IDocumentResultMeta.class );
    document.setName( name );
    document.setDescription( description );
    document.setDocumentType( type );
    document.setPath( path );
    document.setStatus( status );
    if( minValue != null )
      document.setMinValue( minValue );
    if( maxValue != null )
      document.setMaxValue( maxValue );
  }
}
