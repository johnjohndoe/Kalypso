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

/**
 * Factory for creating theme constructors
 * 
 * @author Thomas Jung
 */
public class ThemeConstructionFactory implements IResultControlFactory
{
  private final Map<IResultMeta, AbstractThemeCreator> m_creatorMap = new HashMap<>();

  private final IFolder m_scenarioFolder;

  public ThemeConstructionFactory( final IFolder scenarioFolder )
  {
    m_scenarioFolder = scenarioFolder;
  }

  @Override
  public AbstractThemeCreator createThemeConstructor( final IResultMeta resultMeta )
  {
    // check if already present in map, if yes, return the already existing element
    if( m_creatorMap.containsKey( resultMeta ) )
      return m_creatorMap.get( resultMeta );

    final AbstractThemeCreator constructor = doCreateConstructor( resultMeta );
    m_creatorMap.put( resultMeta, constructor );
    return constructor;
  }

  private AbstractThemeCreator doCreateConstructor( final IResultMeta resultMeta )
  {
    // creates new instances for each IResultMeta type, adds it to the HashMap and returns it
    if( resultMeta instanceof IScenarioResultMeta )
      return new ScenarioResultThemeCreator();

    // StepResults
    if( resultMeta instanceof ICalcUnitResultMeta )
    {
      /* TODO: manage cascading themes, if whole step is selected */
      return new CalcUnitResultThemeCreator();
    }

    // StepResults
    if( resultMeta instanceof IStepResultMeta )
    {
      /* TODO: manage cascading themes, if whole step is selected */
      return new StepResultThemeCreator();
    }

    // DocumentResults
    if( resultMeta instanceof IDocumentResultMeta )
    {
      final IDocumentResultMeta documentResult = (IDocumentResultMeta)resultMeta;
      final DOCUMENTTYPE documentType = documentResult.getDocumentType();

      switch( documentType )
      {
        case tinTerrain:
        case tinDepth:
        case tinVelo:
        case tinWsp:
        case tinShearStress:
        case tinDifference:
          return new TinResultThemeCreator( documentResult, m_scenarioFolder );

        case nodes:
          return new NodeResultThemeCreator( documentResult, m_scenarioFolder );

        case coreDataZip:
        case hydrograph:
        case lengthSection:
        case log:
      }
    }

    return null;
  }
}