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
package org.kalypso.ui.wizards.results;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IFolder;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Factory for creating theme constructors
 *
 * @author Thomas Jung
 *
 */
public class ThemeConstructionFactory implements IThemeConstructionFactory
{
  private final Map<IResultMeta, IResultThemeConstructor> m_creatorMap = new HashMap<>();

  private final IFolder m_scenarioFolder;

  public ThemeConstructionFactory( final IFolder scenarioFolder )
  {
    m_scenarioFolder = scenarioFolder;
  }

  @Override
  public IResultThemeConstructor createThemeConstructor( final IResultMeta resultMeta )
  {
    // check if already present in map, if yes, return the already existing element

    if( m_creatorMap.containsKey( resultMeta ) )
    {
      return m_creatorMap.get( resultMeta );
    }
    else
    // creates new instances for each IResultMeta type, adds it to the HashMap and returns it
    if( resultMeta instanceof IScenarioResultMeta )
    {
      final IScenarioResultMeta scenarioResult = (IScenarioResultMeta) resultMeta;
      final ScenarioResultThemeCreator scenarioResultThemeCreator = new ScenarioResultThemeCreator();
      m_creatorMap.put( scenarioResult, scenarioResultThemeCreator );
      return scenarioResultThemeCreator;
    }

    // StepResults
    else if( resultMeta instanceof ICalcUnitResultMeta )
    {
      final ICalcUnitResultMeta calcUnitResult = (ICalcUnitResultMeta) resultMeta;

      final CalcUnitResultThemeCreator calcUnitResultThemeCreator = new CalcUnitResultThemeCreator();

      final IFeatureBindingCollection<IResultMeta> children = calcUnitResult.getChildren();
      for( final IResultMeta child : children )
      {
        final ThemeConstructionFactory factory = new ThemeConstructionFactory( m_scenarioFolder );
        factory.createThemeConstructor( child );
      }

      /* TODO: manage cascading themes, if whole step is selected */

      // right now, don't put the step result into the map, just its children (documents)
      // m_creatorMap.put( calcUnitResult, calcUnitResultThemeCreator );
      return calcUnitResultThemeCreator;
    }

    // StepResults
    else if( resultMeta instanceof IStepResultMeta )
    {
      final IStepResultMeta stepResult = (IStepResultMeta) resultMeta;

      final StepResultThemeCreator stepResultThemeCreator = new StepResultThemeCreator();

      final IFeatureBindingCollection<IResultMeta> children = stepResult.getChildren();
      for( final IResultMeta child : children )
      {
        final ThemeConstructionFactory factory = new ThemeConstructionFactory( m_scenarioFolder );
        factory.createThemeConstructor( child );
      }

      /* TODO: manage cascading themes, if whole step is selected */

      // right now, don't put the step result into the map, just its children (documents)
      // m_creatorMap.put( stepResult, stepResultThemeCreator );
      return stepResultThemeCreator;

    }

    // DocumentResults
    else if( resultMeta instanceof IDocumentResultMeta )
    {
      final IDocumentResultMeta documentResult = (IDocumentResultMeta) resultMeta;
      final DOCUMENTTYPE documentType = documentResult.getDocumentType();

      if( documentType == DOCUMENTTYPE.tinTerrain || documentType == DOCUMENTTYPE.tinDepth || documentType == DOCUMENTTYPE.tinVelo || documentType == DOCUMENTTYPE.tinWsp
          || documentType == DOCUMENTTYPE.tinShearStress || documentType == DOCUMENTTYPE.tinDifference )
      {
        final TinResultThemeCreator tinResultThemeCreator = new TinResultThemeCreator( documentResult, m_scenarioFolder );
        m_creatorMap.put( documentResult, tinResultThemeCreator );
        return tinResultThemeCreator;
      }
      else if( documentType == DOCUMENTTYPE.nodes )
      {
        final NodeResultThemeCreator nodeResultThemeCreator = new NodeResultThemeCreator( documentResult, m_scenarioFolder );
        m_creatorMap.put( documentResult, nodeResultThemeCreator );
        return nodeResultThemeCreator;
      }
    }

    return null;
  }

}
