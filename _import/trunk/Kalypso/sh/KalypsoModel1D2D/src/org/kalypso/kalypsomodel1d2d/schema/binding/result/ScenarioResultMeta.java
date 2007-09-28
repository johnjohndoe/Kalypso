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

  public ScenarioResultMeta( Feature featureToBind )
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
  public ICalcUnitResultMeta findCalcUnitMetaResult( String calcUnitGmlID )
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
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta#importCalculationUnit(org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta)
   */
  public void updateResultMeta( final ICalcUnitResultMeta newCalcUnitMeta, final boolean isRestart, final boolean isSteadyCalculation, final boolean isUnsteadyCalculation, final Integer restartStep ) throws Exception
  {
    /* the managing of the result db data (documents) should happen somewhere outside the db */
    // look for existing results of this calc unit
    // this cannot happen, but just to be sure...
    if( isRestart && isUnsteadyCalculation && restartStep == null )
      throw new RuntimeException( "Unsteady restart is calculated, but there is no information regarding the restart step. Result database cannot be updated." );

    final ICalcUnitResultMeta oldCalcUnitMeta = findCalcUnitMetaResult( newCalcUnitMeta.getCalcUnit() );
    if( oldCalcUnitMeta != null )
    {
      if( isRestart )
      {
        final IFeatureWrapperCollection<IResultMeta> oldChildren = oldCalcUnitMeta.getChildren();
        final IFeatureWrapperCollection<IResultMeta> newChildren = newCalcUnitMeta.getChildren();
        final List<IResultMeta> oldChildrenToRemove = new ArrayList<IResultMeta>();
        final List<IResultMeta> newChildrenToRemove = new ArrayList<IResultMeta>();
        for( final IResultMeta resultMeta : oldChildren )
        {
          if( resultMeta instanceof IStepResultMeta )
          {
            final IStepResultMeta stepResultMeta = (IStepResultMeta) resultMeta;
            if( isSteadyCalculation && stepResultMeta.getStepNumber() == -1 )
              oldChildrenToRemove.add( resultMeta );
            else if( isUnsteadyCalculation && stepResultMeta.getStepNumber() >= restartStep )
              oldChildrenToRemove.add( resultMeta );
          }
        }
        for( final IResultMeta resultMeta : newChildren )
        {
          if( resultMeta instanceof IDocumentResultMeta )
          {
            final IResultMeta parent = resultMeta.getParent();
            if( !(parent instanceof IStepResultMeta) )
              newChildrenToRemove.add( resultMeta );
          }
        }

        // removing cannot be done directly in previous loop because of ConcurrentModificationException on interator
        for( final IResultMeta resultToRemove : oldChildrenToRemove )
          oldChildren.remove( resultToRemove );
        for( final IResultMeta resultToRemove : newChildrenToRemove )
          newChildren.remove( resultToRemove );

        for( final IResultMeta resultMeta : newChildren )
          oldChildren.cloneInto( resultMeta );
      }
      else
      {
        // no restart
        getChildren().remove( oldCalcUnitMeta );
        getChildren().cloneInto( newCalcUnitMeta );
      }
    }
    else
      getChildren().cloneInto( newCalcUnitMeta );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta#addDocument(java.lang.String,
   *      java.lang.String, org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE,
   *      java.lang.String)
   */
  public void addDocument( String name, String description, DOCUMENTTYPE type, IPath path, IStatus status, BigDecimal minValue, BigDecimal maxValue )
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
